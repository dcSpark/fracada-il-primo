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
    } deriving (Generic, ToJSON, FromJSON, ToSchema, Prelude.Show)


data AddNFT = AddNFT
    {
      an_asset :: AssetClass
    , an_sigs  :: [Signature]
    , an_msg   :: !Builtins.BuiltinByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data MintMore = MintMore
    {
      mm_count :: Integer
    , mm_sigs  :: [Signature]
    , mm_msg   :: !Builtins.BuiltinByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FracNFTSchema = Endpoint "fractionNFT" ToFraction
    .\/ Endpoint "returnNFT" ()
    .\/ Endpoint "addNFT" AddNFT
    .\/ Endpoint "mintMoreTokens" MintMore



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
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint =  Value.singleton currency fractionTokenName fractions
      payBackTokens = mustPayToPubKey pkh tokensToMint
      -- value of NFT
      valueToScript = assetClassValue initTokenClass 1
      -- keep the minted amount and asset class in the datum
      datum = Datum $ toBuiltinData FractionNFTDatum{ tokensClass= assetClass currency fractionTokenName, totalFractions = fractions}

      mintRedeemer = Redeemer $ toBuiltinData fractions
      --build the constraints and submit the transaction
      validator = fractionValidatorScript params
      lookups = Constraints.mintingPolicy mintingScript  <>
                Constraints.otherScript validator
      tx      = Constraints.mustMintValueWithRedeemer mintRedeemer tokensToMint  <>
                Constraints.mustPayToOtherScript (fractionNftValidatorHash params) datum valueToScript <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)

returnNFT :: FractionNFTParameters -> () -> Contract w FracNFTSchema Text ()
returnNFT params@FractionNFTParameters{initTokenClass} _ = do
  -- pay nft to signer
  -- burn tokens
    pkh   <- Contract.ownPubKeyHash
    utxos' <- utxosAt ( fractionNftValidatorAddress params)
    let
      -- declare the NFT value
      valueToWallet = assetClassValue initTokenClass 1
      -- find the UTxO that has the NFT we're looking for
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
      tokensCurrency =  curSymbol params fractionTokenName
      amountToBurn = negate totalFractions
      tokensToBurn =  Value.singleton tokensCurrency fractionTokenName amountToBurn

      emptyRedeemer = Nothing :: Maybe AddToken
      mintRedeemer = Redeemer $ toBuiltinData amountToBurn

      -- build the constraints and submit
      validator = fractionValidatorScript params
      lookups = Constraints.mintingPolicy (mintFractionTokensPolicy params fractionTokenName)  <>
                Constraints.otherScript validator <>
                Constraints.unspentOutputs utxos' <>
                Constraints.unspentOutputs fracTokenUtxos <>
                Constraints.ownPubKeyHash pkh

      tx      = Constraints.mustMintValueWithRedeemer mintRedeemer tokensToBurn <>
                Constraints.mustSpendScriptOutput nftRef ( Redeemer $ toBuiltinData emptyRedeemer ) <>
                Constraints.mustPayToPubKey pkh valueToWallet

    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "burnt %s" (show totalFractions)

-- maybe is already there
valueOfTxs :: Map.Map TxOutRef ChainIndexTxOut -> Value
valueOfTxs utxos =  Prelude.foldl1 (Prelude.<>) $ map (_ciTxOutValue.snd ) $ Map.toList utxos


addNFT :: FractionNFTParameters -> AddNFT -> Contract w FracNFTSchema Text ()
addNFT params AddNFT{an_asset, an_sigs, an_msg} = do
    utxosAtValidator <- utxosAt ( fractionNftValidatorAddress params)
    let
      -- value of NFT
      valueToScript = (valueOfTxs utxosAtValidator) <> assetClassValue an_asset 1
      nftTx = snd.head $ Map.toList utxosAtValidator

    previousDatum <- extractData nftTx
    let
        redeemer = Just AddToken { newToken=an_asset,signatures'=an_sigs, message= Message an_msg }
        validatorScript = fractionNftValidatorInstance params
        tx       = collectFromScript utxosAtValidator redeemer <> mustPayToTheScript previousDatum valueToScript
    void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
    Contract.logInfo @String $ printf "add new tokens %s" (show an_asset)

mintMoreTokens :: FractionNFTParameters -> MintMore -> Contract w FracNFTSchema Text ()
mintMoreTokens params MintMore{mm_count, mm_sigs, mm_msg} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
    pkh   <- Contract.ownPubKeyHash
    utxosAtValidator <- utxosAt ( fractionNftValidatorAddress params)
    FractionNFTDatum{ tokensClass, totalFractions=currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
    let
      fractionTokenName = snd $ unAssetClass tokensClass
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint =  Value.singleton currency fractionTokenName mm_count
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      datum = FractionNFTDatum{ tokensClass, totalFractions = currentFractions + mm_count}
      mintRedeemer = Redeemer $ toBuiltinData mm_count

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator
      redeemer = Just AddToken { newToken=tokensClass,signatures'=mm_sigs, message= Message mm_msg }

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups = Constraints.mintingPolicy mintingScript  <>
                Constraints.unspentOutputs utxosAtValidator <>
                Constraints.typedValidatorLookups validator
      tx      = Constraints.mustMintValueWithRedeemer mintRedeemer tokensToMint  <>
                Constraints.mustPayToTheScript datum valueToScript <>
                collectFromScript utxosAtValidator redeemer <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Fractioning lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

endpoints :: FractionNFTParameters ->  Contract () FracNFTSchema Text ()
endpoints params = forever
                $ handleError logError
                $ awaitPromise
                $ fractionNFT' `select` burn' `select` addNFT' `select` mintMoreTokens'
  where
    fractionNFT' = endpoint @"fractionNFT" $ fractionNFT params
    burn' = endpoint @"returnNFT" $ returnNFT params
    addNFT' = endpoint @"addNFT" $ addNFT params
    mintMoreTokens' = endpoint @"mintMoreTokens" $ mintMoreTokens params

