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
import           Fracada.Constants   as C
import           Fracada.Minting
import           Fracada.Utils
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
    pubKeys           :: [PubKeyHash],
    minSigs           :: Integer,
    initTokenClass    :: AssetClass
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Prelude.Show)

data AddNFT = AddNFT
  { an_asset :: AssetClass,
    an_sigs  :: [PubKeyHash]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MintMore = MintMore
  { mm_count :: Integer,
    mm_sigs  :: [PubKeyHash]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type FracNFTSchema =
  Endpoint "fractionNFT" ToFraction
    .\/ Endpoint "returnNFT" AssetClass
    .\/ Endpoint "addNFT" AddNFT
    .\/ Endpoint "mintMoreTokens" MintMore
    .\/ Endpoint "currentDatumHash" ()

emptyRedeemer :: Redeemer
emptyRedeemer = Redeemer $ toBuiltinData ()

initialMintRedeemer :: TxOutRef -> Redeemer
initialMintRedeemer txOutRef = Redeemer $ toBuiltinData (InitialMint txOutRef)

fractionRedeemer :: Redeemer
fractionRedeemer = Redeemer $ toBuiltinData MintMoreFractions

burnRedeemer :: Redeemer
burnRedeemer = Redeemer $ toBuiltinData Burn

getDatumHash :: ChainIndexTxOut -> Either DatumHash Datum
getDatumHash PublicKeyChainIndexTxOut {} = throwError "no datum for a txout of a public key address"
getDatumHash ScriptChainIndexTxOut {_ciTxOutDatum} = _ciTxOutDatum

toDatum :: PlutusTx.ToData o => o -> Datum
toDatum = Datum . PlutusTx.toBuiltinData

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

fractionNFT :: ToFraction -> Contract w FracNFTSchema Text ()
fractionNFT ToFraction {fractions, pubKeys, minSigs, initTokenClass} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  futxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  let --find the minting script instance
      utxo = fst (head $ Map.toList futxos)
      fractionTokenName = TokenName {unTokenName = calculateFractionTokenNameHash utxo}
      mintingScript = fracadaPolicy

      -- define the fraction tokens to mint and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      fractionTokensToMint = Value.singleton currency fractionTokenName fractions
      payBackTokens = mustPayToPubKey pkh $ fractionTokensToMint <> C.minAda
      -- value of NFT
      nft = assetClassValue initTokenClass 1
      -- define validity token to mint and pay to the script
      validityTokenToMint = Value.singleton currency C.fracadaValidityTokenName 1
      valueToMint = fractionTokensToMint <> validityTokenToMint
      valueToScript = nft <> validityTokenToMint <> C.minAda
      -- keep the minted amount and asset class in the datum
      fractionAsset = assetClass currency fractionTokenName
      datum = Datum $ toBuiltinData FracadaDatum {fractionAC = fractionAsset, emittedFractions = fractions, authorizedPubKeys = pubKeys, minSigRequired = minSigs}

      --build the constraints and submit the transaction
      validator = fractionValidatorScript
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.otherScript validator
      tx =
        Constraints.mustMintValueWithRedeemer (initialMintRedeemer utxo) valueToMint
          <> Constraints.mustPayToOtherScript fracadaValidatorHash datum valueToScript
          <> payBackTokens
          <> mustIncludeDatum datum
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx -- submitTxConstraintsWith @Void lookups tx
  Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)
  Contract.logInfo @String $ printf "pks %s" (show pubKeys)

returnNFT :: AssetClass -> Contract w FracNFTSchema Text ()
returnNFT initTokenClass = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt fracadaValidatorAddress
  let -- declare the NFT value
      valueToWallet = assetClassValue initTokenClass 1
      -- find the UTxO that has the NFT we're looking for
      (nftRef, nftTx) = head $ Map.toList utxos'
  -- use the auxiliary extractData function to get the datum content
  currentDatum@FracadaDatum {fractionAC, emittedFractions} <- extractData nftTx
  -- assuming that all the fraction tokens are in the owner's `ownPubkey` address. For tracing it is good enough,
  -- though for real-use-cases it is more nuanced, as the owner can have them on different
  -- UTxOs.
  futxos <- utxosAt (pubKeyHashAddress pkh Nothing)

  let tokensAsset = AssetClass (tokensCurrency, fractionTokenName)
      fracTokenUtxos = Map.filter (\v -> assetClassValueOf (_ciTxOutValue v) tokensAsset > 0) futxos

      -- declare the fractional tokens to burn
      (_, fractionTokenName) = unAssetClass fractionAC
      tokensCurrency = curSymbol
      amountToBurn = negate emittedFractions
      fractionTokensToBurn = Value.singleton tokensCurrency fractionTokenName amountToBurn

      -- define validity token to burn
      validityToken = Value.singleton tokensCurrency C.fracadaValidityTokenName (-1)

      tokensToBurn = fractionTokensToBurn <> validityToken

      -- build the constraints and submit
      validator = fractionValidatorScript
      lookups =
        Constraints.mintingPolicy fracadaPolicy
          <> Constraints.otherScript validator
          <> Constraints.unspentOutputs utxos'
          <> Constraints.unspentOutputs fracTokenUtxos
          <> Constraints.ownPaymentPubKeyHash pkh

      tx =
        Constraints.mustMintValueWithRedeemer burnRedeemer tokensToBurn
          <> Constraints.mustSpendScriptOutput nftRef unitRedeemer
          <> Constraints.mustPayToPubKey pkh valueToWallet
          <> mustIncludeDatum (toDatum currentDatum)

  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show emittedFractions)

-- maybe is already there
valueOfTxs :: Map.Map TxOutRef ChainIndexTxOut -> Value
valueOfTxs utxos = Prelude.foldl1 (Prelude.<>) $ map (_ciTxOutValue . snd) $ Map.toList utxos

currentDatumHash :: (AsContractError e) => () -> Contract w s e ()
currentDatumHash _ = do
  utxosAtValidator <- utxosAt fracadaValidatorAddress
  let theUtxo = snd . head . Map.toList $ utxosAtValidator
      dh = either id datumHash $ getDatumHash theUtxo
  Contract.logInfo @String $ printf "current datum hash: %s" (show dh)

addNFT :: AddNFT -> Contract w FracNFTSchema Text ()
addNFT AddNFT {an_asset, an_sigs} = do
  utxosAtValidator <- utxosAt fracadaValidatorAddress
  let -- value of NFT
      valueToScript = valueOfTxs utxosAtValidator <> assetClassValue an_asset 1
      nftTx = snd . head $ Map.toList utxosAtValidator

  previousDatum <- extractData nftTx
  let
    validatorScript = fracadaValidatorInstance
    tx =
      collectFromScript utxosAtValidator ()
        <> mustPayToTheScript previousDatum valueToScript
        <> mustIncludeDatum (toDatum previousDatum)
        <> foldMap Constraints.mustBeSignedBy (PaymentPubKeyHash <$> an_sigs)

  void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
  Contract.logInfo @String $ printf "added new NFT %s" (show an_asset)

mintMoreTokens :: MintMore -> Contract w FracNFTSchema Text ()
mintMoreTokens MintMore {mm_count, mm_sigs} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  utxosAtValidator <- utxosAt fracadaValidatorAddress
  currentDatum@FracadaDatum {fractionAC, emittedFractions = currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
  let fractionTokenName = snd $ unAssetClass fractionAC
      --find the minting script instance
      mintingScript = fracadaPolicy

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint = Value.singleton currency fractionTokenName mm_count
      payBackTokens = mustPayToPubKey pkh $ tokensToMint <> minAda

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {emittedFractions = currentFractions + mm_count}

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator

      --build the constraints and submit the transaction
      validator = fracadaValidatorInstance
      lookups =
        mintingPolicy mintingScript
          <> Constraints.unspentOutputs utxosAtValidator
          <> typedValidatorLookups validator
      tx =
        mustMintValueWithRedeemer fractionRedeemer tokensToMint
          <> mustPayToTheScript newDatum valueToScript
          <> mustIncludeDatum (toDatum newDatum)
          <> mustIncludeDatum (toDatum currentDatum)
          <> collectFromScript utxosAtValidator ()
          <> payBackTokens
          <> foldMap Constraints.mustBeSignedBy (PaymentPubKeyHash <$> mm_sigs)
  void $ mkTxConstraints @FracadaValidatorType lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

endpoints :: Contract () FracNFTSchema Text ()
endpoints =
  forever $
    handleError logError $
      awaitPromise $
        fractionNFT' `select` burn' `select` addNFT' `select` mintMoreTokens' -- `select` currentDatumHash'
  where
    fractionNFT' = endpoint @"fractionNFT" fractionNFT
    burn' = endpoint @"returnNFT" returnNFT
    addNFT' = endpoint @"addNFT" addNFT
    mintMoreTokens' = endpoint @"mintMoreTokens" mintMoreTokens

-- currentDatumHash' = endpoint @"currentDatumHash" $ currentDatumHash params
