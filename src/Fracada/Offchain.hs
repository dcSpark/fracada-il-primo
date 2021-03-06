{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Fracada.Offchain where

import           Control.Monad       hiding (fmap)
import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Fracada.Minting
import           Fracada.Validator
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
import qualified Prelude
import           Text.Printf         (printf)

data ToFraction = ToFraction
  { fractions         :: Integer,
    fractionTokenName :: TokenName
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Prelude.Show)

data AddNFT = AddNFT
  { an_asset :: AssetClass,
    an_sigs  :: [Signature]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MintMore = MintMore
  { mm_count :: Integer,
    mm_sigs  :: [Signature]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type FracNFTSchema =
  Endpoint "fractionNFT" ToFraction
    .\/ Endpoint "returnNFT" ()
    .\/ Endpoint "addNFT" AddNFT
    .\/ Endpoint "mintMoreTokens" MintMore
    .\/ Endpoint "currentDatumHash" ()

emptyRedeemer :: Redeemer
emptyRedeemer = Redeemer $ toBuiltinData ()

getDatumHash :: ChainIndexTxOut -> Either DatumHash Datum
getDatumHash PublicKeyChainIndexTxOut {} = throwError "no datum for a txout of a public key address"
getDatumHash ScriptChainIndexTxOut {_ciTxOutDatum} = _ciTxOutDatum

extractData :: (PlutusTx.FromData a) => ChainIndexTxOut -> Contract w s Text a
extractData o = do
  (Datum e) <- either getDatum pure $ getDatumHash o
  maybe
    (throwError "datum hash wrong type")
    pure
    (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case
        Nothing -> throwError "datum not found"
        Just d  -> pure d

fractionNFT :: FractionNFTParameters -> ToFraction -> Contract w FracNFTSchema Text ()
fractionNFT params@FractionNFTParameters {initTokenClass, authorizedPubKeys} ToFraction {fractions, fractionTokenName} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  let --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint = Value.singleton currency fractionTokenName fractions
      payBackTokens = mustPayToPubKey pkh tokensToMint
      -- value of NFT
      valueToScript = assetClassValue initTokenClass 1
      -- keep the minted amount and asset class in the datum
      fractionAsset = assetClass currency fractionTokenName
      datum = Datum $ toBuiltinData FractionNFTDatum {tokensClass = fractionAsset, totalFractions = fractions, newNftClass = fractionAsset}

      --build the constraints and submit the transaction
      validator = fractionValidatorScript params
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.otherScript validator
      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToMint
          <> Constraints.mustPayToOtherScript (fractionNftValidatorHash params) datum valueToScript
          <> payBackTokens
          <> mustIncludeDatum datum
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx -- submitTxConstraintsWith @Void lookups tx
  Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)
  Contract.logInfo @String $ printf "pks %s" (show authorizedPubKeys)

returnNFT :: FractionNFTParameters -> () -> Contract w FracNFTSchema Text ()
returnNFT params@FractionNFTParameters {initTokenClass} _ = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt (fractionNftValidatorAddress params)
  let -- declare the NFT value
      valueToWallet = assetClassValue initTokenClass 1
      -- find the UTxO that has the NFT we're looking for
      (nftRef, nftTx) = head $ Map.toList utxos'
  -- use the auxiliary extractData function to get the datum content
  FractionNFTDatum {tokensClass, totalFractions} <- extractData nftTx
  -- assuming that all the fraction tokens are in the owner's `ownPubkey` address. For tracing it is good enough,
  -- though for real-use-cases it is more nuanced, as the owner can have them on different
  -- UTxOs.
  futxos <- utxosAt (pubKeyHashAddress pkh Nothing)

  let tokensAsset = AssetClass (tokensCurrency, fractionTokenName)
      fracTokenUtxos = Map.filter (\v -> assetClassValueOf (_ciTxOutValue v) tokensAsset > 0) futxos

      -- declare the fractional tokens to burn
      (_, fractionTokenName) = unAssetClass tokensClass
      tokensCurrency = curSymbol params fractionTokenName
      amountToBurn = negate totalFractions
      tokensToBurn = Value.singleton tokensCurrency fractionTokenName amountToBurn

      nothingRedeemer = Nothing :: Maybe AddToken

      -- build the constraints and submit
      validator = fractionValidatorScript params
      lookups =
        Constraints.mintingPolicy (mintFractionTokensPolicy params fractionTokenName)
          <> Constraints.otherScript validator
          <> Constraints.unspentOutputs utxos'
          <> Constraints.unspentOutputs fracTokenUtxos
          <> Constraints.ownPaymentPubKeyHash pkh

      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToBurn
          <> Constraints.mustSpendScriptOutput nftRef (Redeemer $ toBuiltinData nothingRedeemer)
          <> Constraints.mustPayToPubKey pkh valueToWallet

  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show totalFractions)

-- maybe is already there
valueOfTxs :: Map.Map TxOutRef ChainIndexTxOut -> Value
valueOfTxs utxos = Prelude.foldl1 (Prelude.<>) $ map (_ciTxOutValue . snd) $ Map.toList utxos

currentDatumHash :: (AsContractError e) => FractionNFTParameters -> () -> Contract w s e ()
currentDatumHash params _ = do
  utxosAtValidator <- utxosAt (fractionNftValidatorAddress params)
  let theUtxo = snd . head . Map.toList $ utxosAtValidator
      dh = either id datumHash $ getDatumHash theUtxo
  Contract.logInfo @String $ printf "current datum hash: %s" (show dh)

addNFT :: FractionNFTParameters -> AddNFT -> Contract w FracNFTSchema Text ()
addNFT params AddNFT {an_asset, an_sigs} = do
  utxosAtValidator <- utxosAt (fractionNftValidatorAddress params)
  let -- value of NFT
      valueToScript = valueOfTxs utxosAtValidator <> assetClassValue an_asset 1
      nftTx = snd . head $ Map.toList utxosAtValidator

  previousDatum <- extractData nftTx
  let --update datum incrementing the count of nfts
      updatedDatum = previousDatum {newNftClass = an_asset}
      redeemer = Just $ AddToken an_sigs
      validatorScript = fractionNftValidatorInstance params
      tx =
        collectFromScript utxosAtValidator redeemer
          <> mustPayToTheScript updatedDatum valueToScript
          <> mustIncludeDatum (toDatum updatedDatum)

  void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
  Contract.logInfo @String $ printf "added new NFT %s" (show an_asset)

mintMoreTokens :: FractionNFTParameters -> MintMore -> Contract w FracNFTSchema Text ()
mintMoreTokens params MintMore {mm_count, mm_sigs} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  utxosAtValidator <- utxosAt (fractionNftValidatorAddress params)
  currentDatum@FractionNFTDatum {tokensClass, totalFractions = currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
  let fractionTokenName = snd $ unAssetClass tokensClass
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint = Value.singleton currency fractionTokenName mm_count
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {totalFractions = currentFractions + mm_count, newNftClass = tokensClass}

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator
      redeemer = Just $ AddToken mm_sigs

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups =
        mintingPolicy mintingScript
          <> Constraints.unspentOutputs utxosAtValidator
          <> typedValidatorLookups validator
      tx =
        mustMintValueWithRedeemer emptyRedeemer tokensToMint
          <> mustPayToTheScript newDatum valueToScript
          <> mustIncludeDatum (toDatum newDatum)
          <> collectFromScript utxosAtValidator redeemer
          <> payBackTokens
  void $ mkTxConstraints @Fractioning lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

endpoints :: FractionNFTParameters -> Contract () FracNFTSchema Text ()
endpoints params =
  forever $
    handleError logError $
      awaitPromise $
        fractionNFT' `select` burn' `select` addNFT' `select` mintMoreTokens' -- `select` currentDatumHash'
  where
    fractionNFT' = endpoint @"fractionNFT" $ fractionNFT params
    burn' = endpoint @"returnNFT" $ returnNFT params
    addNFT' = endpoint @"addNFT" $ addNFT params
    mintMoreTokens' = endpoint @"mintMoreTokens" $ mintMoreTokens params

-- currentDatumHash' = endpoint @"currentDatumHash" $ currentDatumHash params
