{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Spec.EvilEndpoints where

import           Control.Monad        hiding (fmap)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Fracada.Minting
import           Fracada.Offchain
import           Fracada.Validator
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.IsData
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import           Prelude              (Semigroup (..), String, show)
import           Text.Printf          (printf)

-- try to mint more than what's declared in the datum
extraFractionNFT :: FractionNFTParameters -> ToFraction -> Contract w FracNFTEvilSchema Text ()
extraFractionNFT params@FractionNFTParameters {initTokenClass, authorizedPubKeys} ToFraction {fractions, fractionTokenName} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  let --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint = Value.singleton currency fractionTokenName fractions
      moreTokensToMint = Value.singleton currency fractionTokenName (fractions + 100)
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
        Constraints.mustMintValueWithRedeemer emptyRedeemer moreTokensToMint
          <> Constraints.mustPayToOtherScript (fractionNftValidatorHash params) datum valueToScript
          <> payBackTokens
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)
  Contract.logInfo @String $ printf "pks %s" (show authorizedPubKeys)

-- mint fractional tokens without paying the initial NFT
mintTokensNoNFT :: FractionNFTParameters -> ToFraction -> Contract w FracNFTEvilSchema Text ()
mintTokensNoNFT params@FractionNFTParameters {initTokenClass} ToFraction {fractions, fractionTokenName} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  let --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint = Value.singleton currency fractionTokenName fractions
      payBackTokens = mustPayToPubKey pkh tokensToMint

      --build the constraints and submit the transaction
      validator = fractionValidatorScript params
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.otherScript validator
      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToMint
          <> payBackTokens
  -- ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)

-- return the NFT without burning all the fractional tokens
returnNFTNoFrac :: FractionNFTParameters -> Integer -> Contract w FracNFTEvilSchema Text ()
returnNFTNoFrac params@FractionNFTParameters {initTokenClass} numberToBurn = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt (fractionNftValidatorAddress params)
  let -- declare the NFT value
      valueToWallet = assetClassValue initTokenClass 1
      -- find the UTxO that has the NFT we're looking for
      (nftRef, nftTx) = head $ Map.toList utxos'
  -- use the auxiliary extractData function to get the datum content
  FractionNFTDatum {tokensClass} <- extractData nftTx
  -- assuming that all the fraction tokens are in the owner's `ownPubkey` address. For tracing it is good enough,
  -- though for real-use-cases it is more nuanced, as the owner can have them on different
  -- UTxOs.
  futxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  let tokensAsset = AssetClass (tokensCurrency, fractionTokenName)
      fracTokenUtxos = Map.filter (\v -> assetClassValueOf (_ciTxOutValue v) tokensAsset > 0) futxos

      -- declare the fractional tokens to burn
      (_, fractionTokenName) = unAssetClass tokensClass
      tokensCurrency = curSymbol params fractionTokenName
      amountToBurn = negate numberToBurn
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

  -- ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show amountToBurn)

--add extra NFT than the ones signed
addMoreNFT :: FractionNFTParameters -> AddNFT -> Contract w FracNFTEvilSchema Text ()
addMoreNFT params AddNFT {an_asset, an_sigs} = do
  utxosAtValidator <- utxosAt (fractionNftValidatorAddress params)
  let -- value of NFT
      valueToScript = valueOfTxs utxosAtValidator <> assetClassValue an_asset 2
      nftTx = snd . head $ Map.toList utxosAtValidator

  previousDatum <- extractData nftTx
  let --update datum incrementing the count of nfts
      updatedDatum = previousDatum {newNftClass = an_asset}
      redeemer = Just $ AddToken an_sigs
      validatorScript = fractionNftValidatorInstance params
      tx = collectFromScript utxosAtValidator redeemer <> mustPayToTheScript updatedDatum valueToScript

  void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
  Contract.logInfo @String $ printf "added new NFT %s" (show an_asset)

-- mint more fractional tokens than the ones signed
mintExtraTokens :: FractionNFTParameters -> MintMore -> Contract w FracNFTEvilSchema Text ()
mintExtraTokens params MintMore {mm_count, mm_sigs} = do
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
      tokensToMint = Value.singleton currency fractionTokenName (mm_count + 1)
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {totalFractions = currentFractions + mm_count}

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator
      redeemer = Just $ AddToken mm_sigs

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.unspentOutputs utxosAtValidator
          <> Constraints.typedValidatorLookups validator
      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToMint
          <> Constraints.mustPayToTheScript newDatum valueToScript
          <> collectFromScript utxosAtValidator redeemer
          <> payBackTokens
  void $ mkTxConstraints @Fractioning lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

-- try to take out an NFT while minting
mintTokensStealNft :: FractionNFTParameters -> (MintMore, AssetClass) -> Contract w FracNFTEvilSchema Text ()
mintTokensStealNft params (MintMore {mm_count, mm_sigs}, nftToSteal) = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  utxosAtValidator <- utxosAt (fractionNftValidatorAddress params)
  currentDatum@FractionNFTDatum {tokensClass, totalFractions = currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
  let fractionTokenName = snd $ unAssetClass tokensClass
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      stealValue = assetClassValue nftToSteal 1
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint = Value.singleton currency fractionTokenName (mm_count)
      payBackTokens = mustPayToPubKey pkh $ tokensToMint <> stealValue -- 1 that the wallet already has + 1 in the contract

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {totalFractions = currentFractions + mm_count}

      -- preserve NFTs
      valueToScript = unionWith (-) (valueOfTxs utxosAtValidator) (assetClassValue nftToSteal 1)
      redeemer = Just $ AddToken mm_sigs

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.unspentOutputs utxosAtValidator
          <> Constraints.typedValidatorLookups validator
      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToMint
          <> Constraints.mustPayToTheScript newDatum valueToScript
          <> collectFromScript utxosAtValidator redeemer
          <> payBackTokens
  void $ mkTxConstraints @Fractioning lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

{-# INLINEABLE anyMintScript #-}
anyMintScript :: () -> ScriptContext -> Bool
anyMintScript _ _ = True

anyMintScriptPolicy :: MintingPolicy
anyMintScriptPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy $ anyMintScript||])

anyMintCurSymbol :: CurrencySymbol
anyMintCurSymbol = scriptCurrencySymbol $ anyMintScriptPolicy

-- Mint another asset class at the same time
mintVariousTokens :: FractionNFTParameters -> MintMore -> Contract w FracNFTEvilSchema Text ()
mintVariousTokens params MintMore {mm_count, mm_sigs} = do
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
      extraTokens = Value.singleton anyMintCurSymbol "EXTRA" 10
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {totalFractions = currentFractions + mm_count}

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator
      redeemer = Just $ AddToken mm_sigs

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.mintingPolicy anyMintScriptPolicy
          <> Constraints.unspentOutputs utxosAtValidator
          <> Constraints.typedValidatorLookups validator
      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToMint
          <> Constraints.mustMintValueWithRedeemer (Redeemer $ toBuiltinData ()) extraTokens
          <> Constraints.mustPayToTheScript newDatum valueToScript
          <> collectFromScript utxosAtValidator redeemer
          <> payBackTokens
  void $ mkTxConstraints @Fractioning lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

-- Add NFT without updating the datum
addNFTNoDatumUpd :: FractionNFTParameters -> AddNFT -> Contract w FracNFTEvilSchema Text ()
addNFTNoDatumUpd params AddNFT {an_asset, an_sigs} = do
  utxosAtValidator <- utxosAt (fractionNftValidatorAddress params)
  let -- value of NFT
      valueToScript = (valueOfTxs utxosAtValidator) <> assetClassValue an_asset 1
      nftTx = snd . head $ Map.toList utxosAtValidator

  previousDatum <- extractData nftTx
  let --update datum incrementing the count of nfts
      updatedDatum = previousDatum
      redeemer = Just $ AddToken an_sigs
      validatorScript = fractionNftValidatorInstance params
      tx = collectFromScript utxosAtValidator redeemer <> mustPayToTheScript updatedDatum valueToScript

  void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
  Contract.logInfo @String $ printf "added new NFT %s" (show an_asset)

-- Keep an NFT in the contract after burning all fractional tokens
partialReturn :: FractionNFTParameters -> () -> Contract w FracNFTEvilSchema Text ()
partialReturn params@FractionNFTParameters {initTokenClass} _ = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt (fractionNftValidatorAddress params)
  let -- declare the NFT value
      valueToWallet = assetClassValue initTokenClass 1
      -- find the UTxO that has the NFT we're looking for
      (nftRef, nftTx) = head $ Map.toList utxos'
  -- use the auxiliary extractData function to get the datum content
  currentDatum@FractionNFTDatum {tokensClass, totalFractions, newNftClass} <- extractData nftTx
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

      valueKept = assetClassValue newNftClass 1

      -- build the constraints and submit
      validator = fractionNftValidatorInstance params
      lookups =
        Constraints.mintingPolicy (mintFractionTokensPolicy params fractionTokenName)
          <> Constraints.unspentOutputs utxos'
          <> Constraints.unspentOutputs fracTokenUtxos
          <> Constraints.ownPaymentPubKeyHash pkh
          <> Constraints.typedValidatorLookups validator

      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToBurn
          <> Constraints.mustSpendScriptOutput nftRef (Redeemer $ toBuiltinData nothingRedeemer)
          <> Constraints.mustPayToPubKey pkh valueToWallet
          <> Constraints.mustPayToTheScript currentDatum valueKept

  void $ mkTxConstraints @Fractioning lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show totalFractions)

returnNoNFT :: FractionNFTParameters -> () -> Contract w FracNFTEvilSchema Text ()
returnNoNFT params _ = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt (fractionNftValidatorAddress params)
  let -- find the UTxO that has the NFT we're looking for
      nftTx = snd $ head $ Map.toList utxos'
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

      -- build the constraints and submit
      validator = fractionValidatorScript params
      lookups =
        Constraints.mintingPolicy (mintFractionTokensPolicy params fractionTokenName)
          <> Constraints.otherScript validator
          <> Constraints.unspentOutputs utxos'
          <> Constraints.unspentOutputs fracTokenUtxos
          <> Constraints.ownPaymentPubKeyHash pkh

      tx = Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToBurn
  -- <>
  -- Constraints.mustSpendScriptOutput nftRef ( Redeemer $ toBuiltinData emptyRedeemer ) <>
  -- Constraints.mustPayToPubKey pkh valueToWallet

  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show totalFractions)

type FracNFTEvilSchema =
  Endpoint "extraFractionNFT" ToFraction
    .\/ Endpoint "mintTokensNoNFT" ToFraction
    .\/ Endpoint "returnNFTNoFrac" Integer
    .\/ Endpoint "addMoreNFT" AddNFT
    .\/ Endpoint "mintExtraTokens" MintMore
    .\/ Endpoint "mintTokensStealNft" (MintMore, AssetClass)
    .\/ Endpoint "mintVariousTokens" MintMore
    .\/ Endpoint "addNFTNoDatumUpd" AddNFT
    .\/ Endpoint "partialReturn" ()
    .\/ Endpoint "returnNoNFT" ()

endpoints :: FractionNFTParameters -> Contract () FracNFTEvilSchema Text ()
endpoints params =
  forever $
    handleError logError $
      awaitPromise $
        extraFractionNFT' `select` mintTokensNoNFT' `select` burn' `select` addNFT' `select` mintMoreTokens'
          `select` mintSteal'
          `select` mintVariousTokens'
          `select` addNFTNoDatumUpd'
          `select` partialReturn'
          `select` returnNoNFT'
  where
    extraFractionNFT' = endpoint @"extraFractionNFT" $ extraFractionNFT params
    mintTokensNoNFT' = endpoint @"mintTokensNoNFT" $ mintTokensNoNFT params
    burn' = endpoint @"returnNFTNoFrac" $ returnNFTNoFrac params
    addNFT' = endpoint @"addMoreNFT" $ addMoreNFT params
    mintMoreTokens' = endpoint @"mintExtraTokens" $ mintExtraTokens params
    mintSteal' = endpoint @"mintTokensStealNft" $ mintTokensStealNft params
    mintVariousTokens' = endpoint @"mintVariousTokens" $ mintVariousTokens params
    addNFTNoDatumUpd' = endpoint @"addNFTNoDatumUpd" $ addNFTNoDatumUpd params
    partialReturn' = endpoint @"partialReturn" $ partialReturn params
    returnNoNFT' = endpoint @"returnNoNFT" $ returnNoNFT params

-- reuse signature while adding a different nft
-- reuse signature while minting fract
