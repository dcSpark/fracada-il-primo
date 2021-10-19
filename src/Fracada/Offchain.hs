{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Fracada.Offchain where

import           Control.Monad       hiding (fmap)
import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           GHC.Generics        (Generic)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import           Ledger.Value        as Value
import           Playground.Contract (ToSchema)
import           Plutus.Contract     as Contract
import qualified PlutusTx
import           PlutusTx.Builtins   as Builtins
import           PlutusTx.IsData
import           PlutusTx.Prelude    hiding (Semigroup (..), unless)
import           Prelude             (Semigroup (..), String, show)
import           Text.Printf         (printf)

import           Fracada.Minting
import           Fracada.Validator
import qualified Prelude             as Prelude

data ToFraction = ToFraction
    {
      fractions         :: Integer
    , fractionTokenName :: TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


data AddNFT = AddNFT
    {
      asset :: AssetClass
    , sigs  :: [Signature]
    , msg   :: !Builtins.BuiltinByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FracNFTSchema = Endpoint "1-fractionNFT" ToFraction
    .\/ Endpoint "2-returnNFT" ()
    .\/ Endpoint "3-addNFT" AddNFT



extractData :: (PlutusTx.FromData a) => ChainIndexTxOut -> Contract w s Text a
extractData o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutDatum } -> do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d

fractionNFT :: FractionNFTParameters -> ToFraction -> Contract w FracNFTSchema Text ()
fractionNFT params@FractionNFTParameters{initTokenClass} ToFraction {fractions, fractionTokenName} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
    pkh   <- Contract.ownPubKeyHash
    let

      --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractions fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint =  Value.singleton currency fractionTokenName fractions
      payBackTokens = mustPayToPubKey pkh tokensToMint
      -- value of NFT
      valueToScript = assetClassValue initTokenClass 1
      -- keep the minted amount and asset class in the datum
      datum = Datum $ toBuiltinData FractionNFTDatum{ tokensClass= assetClass currency fractionTokenName, totalFractions = fractions}

      --build the constraints and submit the transaction
      validator = fractionValidatorScript params
      lookups = Constraints.mintingPolicy mintingScript  <>
                Constraints.otherScript validator
      tx      = Constraints.mustMintValue tokensToMint <>
                Constraints.mustPayToOtherScript (fractionNftValidatorHash params) datum valueToScript <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show fractions)

returnNFT :: FractionNFTParameters -> () -> Contract w FracNFTSchema Text ()
returnNFT params@FractionNFTParameters{initTokenClass} _ = do
  -- pay nft to signer
  -- burn tokens
    pkh   <- Contract.ownPubKeyHash
    utxos <- utxosAt $ fractionNftValidatorAddress params
    let
      -- declare the NFT value
      valueToWallet = assetClassValue initTokenClass 1
      -- find the UTxO that has the NFT we're looking for
      utxos' = Map.filter (\v -> 1 == assetClassValueOf (_ciTxOutValue v) initTokenClass ) utxos
      (nftRef,nftTx) = head $ Map.toList utxos'
    -- use the auxiliary extractData function to get the datum content
    FractionNFTDatum {tokensClass, totalFractions } <- extractData nftTx
    -- assuming that all the fraction tokens are in the owner's `ownPubkey` address. For tracing it is good enough,
    -- though for real-use-cases it is more nuanced, as the owner can have them on different
    -- UTxOs.
    ownAddress <- pubKeyHashAddress <$> Contract.ownPubKeyHash
    futxos <- utxosAt ownAddress

    let
      tokensAsset = AssetClass (tokensCurrency, fractionTokenName)
      fracTokenUtxos = Map.filter (\v -> assetClassValueOf (_ciTxOutValue v) tokensAsset > 0  ) futxos

      -- declare the fractional tokens to burn
      (_, fractionTokenName) = unAssetClass tokensClass
      tokensCurrency =  curSymbol params totalFractions fractionTokenName
      tokensToBurn =  Value.singleton tokensCurrency fractionTokenName $ negate totalFractions

      emptyRedeemer = Nothing :: Maybe AddToken

      -- build the constraints and submit
      validator = fractionValidatorScript params
      lookups = Constraints.mintingPolicy (mintFractionTokensPolicy params totalFractions fractionTokenName)  <>
                Constraints.otherScript validator <>
                Constraints.unspentOutputs utxos' <>
                Constraints.unspentOutputs fracTokenUtxos <>
                Constraints.ownPubKeyHash pkh

      tx      = Constraints.mustMintValue tokensToBurn <>
                Constraints.mustSpendScriptOutput nftRef ( Redeemer $ toBuiltinData emptyRedeemer ) <>
                Constraints.mustPayToPubKey pkh valueToWallet

    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "burnt %s" (show totalFractions)


addNFT :: FractionNFTParameters -> AddNFT -> Contract w FracNFTSchema Text ()
addNFT params@FractionNFTParameters{initTokenClass} AddNFT{asset, sigs, msg} = do
    utxos <- utxosAt $ fractionNftValidatorAddress params
    let
      -- value of NFT

      prevValue = Prelude.foldl1 (Prelude.<>) $ map (_ciTxOutValue.snd ) $ Map.toList utxos
      valueToScript = prevValue <> assetClassValue asset 1
      utxos' = Map.filter (\v -> 1 == assetClassValueOf (_ciTxOutValue v) initTokenClass ) utxos
      (nftRef,nftTx) = head $ Map.toList utxos'

      validatorScript = fractionNftValidatorInstance params

    previousDatum <- extractData nftTx
    utxosAtValidator <- utxosAt $ fractionNftValidatorAddress params
    let
        redeemer = Just AddToken { newToken=asset,signatures'=sigs, message= Message msg }
        tx       = collectFromScript utxosAtValidator redeemer <> mustPayToTheScript previousDatum valueToScript
    void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
    Contract.logInfo @String $ printf "add new tokens %s" (show asset)

endpoints :: FractionNFTParameters ->  Contract () FracNFTSchema Text ()
endpoints params = forever
                $ handleError logError
                $ awaitPromise
                $ fractionNFT' `select` burn' `select` addNFT'
  where
    fractionNFT' = endpoint @"1-fractionNFT" $ fractionNFT params
    burn' = endpoint @"2-returnNFT" $ returnNFT params
    addNFT' = endpoint @"3-addNFT" $ addNFT params

