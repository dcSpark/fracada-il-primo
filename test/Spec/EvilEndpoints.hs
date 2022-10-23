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
import           Fracada.Constants    as C
import           Fracada.Minting
import           Fracada.Offchain
import           Fracada.Utils
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
import           Plutus.V1.Ledger.Ada (lovelaceValueOf, Ada (getLovelace))
import           Spec.EvilValidator

-- try to mint more than what's declared in the datum
extraFractionNFT :: ToFraction -> Contract w FracNFTEvilSchema Text ()
extraFractionNFT ToFraction {fractions, pubKeys, minSigs, initTokenClass} = do
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
      moreTokensToMint = Value.singleton currency fractionTokenName (fractions + 100)
      payBackTokens = mustPayToPubKey pkh $ fractionTokensToMint <> C.minAda
      -- value of NFT
      nft = assetClassValue initTokenClass 1
      -- define validity token to mint and pay to the script
      validityTokenToMint = Value.singleton currency C.fracadaValidityTokenName 1
      -- define value to mint
      valueToMint = moreTokensToMint <> validityTokenToMint
      -- define value to to be paid to the script
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
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)
  Contract.logInfo @String $ printf "pks %s" (show pubKeys)

-- record fraudulent fraction tokens class
fractionNFTRecordFraudTokensClass :: ToFraction -> Contract w FracNFTEvilSchema Text ()
fractionNFTRecordFraudTokensClass ToFraction {fractions, pubKeys, minSigs, initTokenClass} = do
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
      -- define value to mint
      valueToMint = fractionTokensToMint <> validityTokenToMint
      -- define value to to be paid to the script
      valueToScript = nft <> validityTokenToMint <> C.minAda

      -- keep the minted amount and asset class in the datum
      fraudCurrency = scriptCurrencySymbol anyMintScriptPolicy
      fraudFractionAsset = assetClass fraudCurrency fractionTokenName
      datum = Datum $ toBuiltinData FracadaDatum {fractionAC = fraudFractionAsset, emittedFractions = fractions, authorizedPubKeys = pubKeys, minSigRequired = minSigs}

      --build the constraints and submit the transaction
      validator = fractionValidatorScript
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.otherScript validator
      tx =
        Constraints.mustMintValueWithRedeemer (initialMintRedeemer utxo) valueToMint
          <> Constraints.mustPayToOtherScript fracadaValidatorHash datum valueToScript
          <> payBackTokens
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s %s for NFT %s" (show fractions) (show $ assetClass currency fractionTokenName) (show initTokenClass)
  Contract.logInfo @String $ printf "recorded %s as fractionAC in datum" (show fraudFractionAsset)
  Contract.logInfo @String $ printf "pks %s" (show pubKeys)

-- mint fraudulent fractional tokens
mintFraudTokens :: ToFraction -> Contract w FracNFTEvilSchema Text ()
mintFraudTokens ToFraction {fractions, initTokenClass} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  futxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  let utxo = fst (head $ Map.toList futxos)
      fractionTokenName = TokenName {unTokenName = calculateFractionTokenNameHash utxo}
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol anyMintScriptPolicy
      tokensToMint = Value.singleton currency fractionTokenName fractions
      payBackTokens = mustPayToPubKey pkh tokensToMint

      --build the constraints and submit the transaction
      validator = fractionValidatorScript
      lookups =
        Constraints.mintingPolicy anyMintScriptPolicy
          <> Constraints.otherScript validator
      tx =
        Constraints.mustMintValueWithRedeemer emptyRedeemer tokensToMint
          <> payBackTokens
  -- ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s %s for NFT %s" (show fractions) (show $ assetClass currency fractionTokenName) (show initTokenClass)

-- burn fraudulent fractional tokens and return NFT
returnNFTFraudFrac :: AssetClass -> Contract w FracNFTEvilSchema Text ()
returnNFTFraudFrac initTokenClass = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt fracadaValidatorAddress
  if null (Map.toList utxos')
      then logError @String $ "UTxO with always succeeding validator not found!"
      else do
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
        tokensCurrency = anyMintCurSymbol
        amountToBurn = negate emittedFractions
        fractionTokensToBurn = Value.singleton tokensCurrency fractionTokenName amountToBurn

        -- define validity token to burn
        validityToken = Value.singleton tokensCurrency C.fracadaValidityTokenName (-1)

        tokensToBurn = fractionTokensToBurn <> validityToken

        -- build the constraints and submit
        validator = fractionValidatorScript
        lookups =
          Constraints.mintingPolicy anyMintScriptPolicy
            <> Constraints.otherScript validator
            <> Constraints.unspentOutputs utxos'
            <> Constraints.unspentOutputs fracTokenUtxos
            <> Constraints.ownPaymentPubKeyHash pkh

        tx =
          Constraints.mustMintValueWithRedeemer burnRedeemer tokensToBurn
            <> Constraints.mustSpendScriptOutput nftRef emptyRedeemer
            <> Constraints.mustPayToPubKey pkh valueToWallet
            <> mustIncludeDatum (toDatum currentDatum)

    void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    Contract.logInfo @String $ printf "return NFT while burning %s %s" (show emittedFractions) (show $ assetClass tokensCurrency fractionTokenName)

-- try to mint more than what's declared in the datum
extraFractionNFTWithArbitraryDatumUtxo :: ToFraction -> Contract w FracNFTEvilSchema Text ()
extraFractionNFTWithArbitraryDatumUtxo ToFraction {fractions, pubKeys, minSigs, initTokenClass} = do
-- construct utxo with always succeeding validator
  let v = lovelaceValueOf $ getLovelace minAdaTxOut
      txn = Constraints.mustPayToTheScript 9 v
  ledgerTx <- submitTxConstraints alwaysSucceedsTypedValidator txn
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- utxosAt alwaysSucceedsScrAddress
  futxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  if null (Map.toList utxos)
    then logError @String $ "UTxO with always succeeding validator not found!"
    else do
      let utxo = head (Map.toList utxos)
          --find the minting script instance
          futxo = fst (head $ Map.toList futxos)
          fractionTokenName = TokenName {unTokenName = calculateFractionTokenNameHash futxo}
          mintingScript = fracadaPolicy

          -- define the fraction tokens to mint and be paid to signer
          currency = scriptCurrencySymbol mintingScript
          moreTokensToMint = Value.singleton currency fractionTokenName (fractions + 100)
          payBackTokens = mustPayToPubKey pkh $ moreTokensToMint <> C.minAda
          -- value of NFT
          nft = assetClassValue initTokenClass 1
          -- define validity token to mint and pay to the script
          validityTokenToMint = Value.singleton currency C.fracadaValidityTokenName 1
          -- define value to mint
          valueToMint = moreTokensToMint <> validityTokenToMint
          -- define value to to be paid to the script
          valueToScript = nft <> validityTokenToMint <> C.minAda
          -- keep the minted amount and asset class in the datum
          fractionAsset = assetClass currency fractionTokenName
          datum = Datum $ toBuiltinData FracadaDatum {fractionAC = fractionAsset, emittedFractions = fractions, authorizedPubKeys = pubKeys, minSigRequired = minSigs}

          --build the constraints and submit the transaction
          validator = alwaysSucceedsValidatorScript
          lookups =
            Constraints.unspentOutputs (uncurry Map.singleton utxo)
              <> Constraints.mintingPolicy mintingScript
              <> Constraints.otherScript validator
          tx =
            Constraints.mustMintValueWithRedeemer (initialMintRedeemer futxo) valueToMint
              <> Constraints.mustPayToOtherScript fracadaValidatorHash datum valueToScript
              <> payBackTokens
              <> Constraints.mustSpendScriptOutput (fst utxo) emptyRedeemer
      void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
      Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)
      Contract.logInfo @String $ printf "pks %s" (show pubKeys)

-- mint fractional tokens without creating the contract UTxO
mintTokensNoContract :: ToFraction -> Contract w FracNFTEvilSchema Text ()
mintTokensNoContract ToFraction {fractions, initTokenClass} = do
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
      -- define validity token to mint and pay to the script
      validityTokenToMint = Value.singleton currency C.fracadaValidityTokenName 1
      -- define value to mint
      valueToMint = fractionTokensToMint <> validityTokenToMint

      --build the constraints and submit the transaction
      validator = fractionValidatorScript
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.otherScript validator
      tx =
        Constraints.mustMintValueWithRedeemer (initialMintRedeemer utxo) valueToMint
          <> payBackTokens
  -- ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)

-- return the NFT without burning all the fractional tokens
returnNFTNoFrac :: (Integer, AssetClass) -> Contract w FracNFTEvilSchema Text ()
returnNFTNoFrac (numberToBurn, initTokenClass) = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt fracadaValidatorAddress
  let -- declare the NFT value
      valueToWallet = assetClassValue initTokenClass 1
      -- find the UTxO that has the NFT we're looking for
      (nftRef, nftTx) = head $ Map.toList utxos'
  -- use the auxiliary extractData function to get the datum content
  currentDatum@FracadaDatum {fractionAC} <- extractData nftTx
  -- assuming that all the fraction tokens are in the owner's `ownPubkey` address. For tracing it is good enough,
  -- though for real-use-cases it is more nuanced, as the owner can have them on different
  -- UTxOs.
  futxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  let tokensAsset = AssetClass (tokensCurrency, fractionTokenName)
      fracTokenUtxos = Map.filter (\v -> assetClassValueOf (_ciTxOutValue v) tokensAsset > 0) futxos

      -- declare the fractional tokens to burn
      (_, fractionTokenName) = unAssetClass fractionAC
      tokensCurrency = curSymbol
      amountToBurn = negate numberToBurn
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
          <> Constraints.mustSpendScriptOutput nftRef emptyRedeemer
          <> Constraints.mustPayToPubKey pkh valueToWallet
          <> mustIncludeDatum (toDatum currentDatum)

  -- ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show amountToBurn)

--add extra NFT than the ones signed
stealWhenAdding :: AddNFT -> Contract w FracNFTEvilSchema Text ()
stealWhenAdding AddNFT {an_asset, an_sigs} = do
  utxosAtValidator <- utxosAt fracadaValidatorAddress
  let -- value of NFT
      valueToScript = valueOfTxs utxosAtValidator <> assetClassValue an_asset (-1)
      nftTx = snd . head $ Map.toList utxosAtValidator

  previousDatum <- extractData nftTx
  let
      validatorScript = fracadaValidatorInstance
      tx = collectFromScript utxosAtValidator () <> mustPayToTheScript previousDatum valueToScript
        <> foldMap Constraints.mustBeSignedBy (PaymentPubKeyHash <$> an_sigs)

  void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
  Contract.logInfo @String $ printf "added new NFT %s" (show an_asset)

-- mint more fractional tokens than the ones signed
mintExtraTokens :: MintMore -> Contract w FracNFTEvilSchema Text ()
mintExtraTokens MintMore {mm_count, mm_sigs} = do
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
      tokensToMint = Value.singleton currency fractionTokenName (mm_count + 1)
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {emittedFractions = currentFractions + mm_count}

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator

      --build the constraints and submit the transaction
      validator = fracadaValidatorInstance
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.unspentOutputs utxosAtValidator
          <> Constraints.typedValidatorLookups validator
      tx =
        Constraints.mustMintValueWithRedeemer fractionRedeemer tokensToMint
          <> Constraints.mustPayToTheScript newDatum valueToScript
          <> collectFromScript utxosAtValidator ()
          <> payBackTokens
          <> foldMap Constraints.mustBeSignedBy (PaymentPubKeyHash <$> mm_sigs)
          <> mustIncludeDatum (toDatum currentDatum)
  void $ mkTxConstraints @FracadaValidatorType lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

-- try to take out an NFT while minting
mintTokensStealNft :: (MintMore, AssetClass) -> Contract w FracNFTEvilSchema Text ()
mintTokensStealNft (MintMore {mm_count, mm_sigs}, nftToSteal) = do
  -- pay nft to contract
  -- pay minted tokens back to signer
  pkh <- Contract.ownPaymentPubKeyHash
  utxosAtValidator <- utxosAt fracadaValidatorAddress
  currentDatum@FracadaDatum {fractionAC, emittedFractions = currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
  let fractionTokenName = snd $ unAssetClass fractionAC
      --find the minting script instance
      mintingScript = fracadaPolicy

      stealValue = assetClassValue nftToSteal 1
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint = Value.singleton currency fractionTokenName (mm_count)
      payBackTokens = mustPayToPubKey pkh $ tokensToMint <> stealValue -- 1 that the wallet already has + 1 in the contract

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {emittedFractions = currentFractions + mm_count}

      -- preserve NFTs
      valueToScript = unionWith (-) (valueOfTxs utxosAtValidator) (assetClassValue nftToSteal 1)

      --build the constraints and submit the transaction
      validator = fracadaValidatorInstance
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.unspentOutputs utxosAtValidator
          <> Constraints.typedValidatorLookups validator
      tx =
        Constraints.mustMintValueWithRedeemer fractionRedeemer tokensToMint
          <> Constraints.mustPayToTheScript newDatum valueToScript
          <> collectFromScript utxosAtValidator ()
          <> payBackTokens
          <> foldMap Constraints.mustBeSignedBy (PaymentPubKeyHash <$> mm_sigs)
          <> mustIncludeDatum (toDatum currentDatum)
  void $ mkTxConstraints @FracadaValidatorType lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
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
mintVariousTokens :: MintMore -> Contract w FracNFTEvilSchema Text ()
mintVariousTokens MintMore {mm_count, mm_sigs} = do
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
      extraTokens = Value.singleton anyMintCurSymbol "EXTRA" 10
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum {emittedFractions = currentFractions + mm_count}

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator

      --build the constraints and submit the transaction
      validator = fracadaValidatorInstance
      lookups =
        Constraints.mintingPolicy mintingScript
          <> Constraints.mintingPolicy anyMintScriptPolicy
          <> Constraints.unspentOutputs utxosAtValidator
          <> Constraints.typedValidatorLookups validator
      tx =
        Constraints.mustMintValueWithRedeemer fractionRedeemer tokensToMint
          <> Constraints.mustMintValueWithRedeemer (Redeemer $ toBuiltinData ()) extraTokens
          <> Constraints.mustPayToTheScript newDatum valueToScript
          <> collectFromScript utxosAtValidator ()
          <> payBackTokens
          <> foldMap Constraints.mustBeSignedBy (PaymentPubKeyHash <$> mm_sigs)
          <> mustIncludeDatum (toDatum currentDatum)
  void $ mkTxConstraints @FracadaValidatorType lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)

-- Add NFT without updating the datum
addNFTChangeDatum :: AddNFT -> Contract w FracNFTEvilSchema Text ()
addNFTChangeDatum AddNFT {an_asset, an_sigs} = do
  utxosAtValidator <- utxosAt fracadaValidatorAddress
  let -- value of NFT
      valueToScript = (valueOfTxs utxosAtValidator) <> assetClassValue an_asset 1
      nftTx = snd . head $ Map.toList utxosAtValidator

  previousDatum <- extractData nftTx
  let --update datum incrementing the count of nfts
      updatedDatum = previousDatum {emittedFractions = emittedFractions previousDatum + 1}
      validatorScript = fracadaValidatorInstance
      tx = collectFromScript utxosAtValidator () <> mustPayToTheScript updatedDatum valueToScript
        <> foldMap Constraints.mustBeSignedBy (PaymentPubKeyHash <$> an_sigs)

  void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
  Contract.logInfo @String $ printf "added new NFT %s" (show an_asset)

-- Keep an NFT in the contract after burning all fractional tokens
partialReturn :: (AssetClass, AssetClass) -> Contract w FracNFTEvilSchema Text ()
partialReturn (newNftClass, initTokenClass) = do
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

      valueKept = assetClassValue newNftClass 1

      -- build the constraints and submit
      validator = fracadaValidatorInstance
      lookups =
        Constraints.mintingPolicy fracadaPolicy
          <> Constraints.unspentOutputs utxos'
          <> Constraints.unspentOutputs fracTokenUtxos
          <> Constraints.ownPaymentPubKeyHash pkh
          <> Constraints.typedValidatorLookups validator

      tx =
        Constraints.mustMintValueWithRedeemer burnRedeemer tokensToBurn
          <> Constraints.mustSpendScriptOutput nftRef emptyRedeemer
          <> Constraints.mustPayToPubKey pkh valueToWallet
          <> Constraints.mustPayToTheScript currentDatum valueKept
          <> mustIncludeDatum (toDatum currentDatum)

  void $ mkTxConstraints @FracadaValidatorType lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show emittedFractions)

returnNoNFT :: () -> Contract w FracNFTEvilSchema Text ()
returnNoNFT _ = do
  -- pay nft to signer
  -- burn tokens
  pkh <- Contract.ownPaymentPubKeyHash
  utxos' <- utxosAt fracadaValidatorAddress
  let -- find the UTxO that has the NFT we're looking for
      nftTx = snd $ head $ Map.toList utxos'
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
      tokensToBurn = Value.singleton tokensCurrency fractionTokenName amountToBurn

      -- build the constraints and submit
      validator = fractionValidatorScript
      lookups =
        Constraints.mintingPolicy fracadaPolicy
          <> Constraints.otherScript validator
          <> Constraints.unspentOutputs utxos'
          <> Constraints.unspentOutputs fracTokenUtxos
          <> Constraints.ownPaymentPubKeyHash pkh

      tx = Constraints.mustMintValueWithRedeemer burnRedeemer tokensToBurn
          <> mustIncludeDatum (toDatum currentDatum)
  -- <>
  -- Constraints.mustSpendScriptOutput nftRef ( Redeemer $ toBuiltinData burnRedeemer ) <>
  -- Constraints.mustPayToPubKey pkh valueToWallet

  void $ mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  Contract.logInfo @String $ printf "burnt %s" (show emittedFractions)

type FracNFTEvilSchema =
  Endpoint "extraFractionNFT" ToFraction
    .\/ Endpoint "fractionNFTRecordFraudTokensClass" ToFraction
    .\/ Endpoint "mintFraudTokens" ToFraction
    .\/ Endpoint "extraFractionNFTWithArbitraryDatumUtxo" ToFraction
    .\/ Endpoint "mintTokensNoContract" ToFraction
    .\/ Endpoint "returnNFTNoFrac" (Integer, AssetClass)
    .\/ Endpoint "stealWhenAdding" AddNFT
    .\/ Endpoint "mintExtraTokens" MintMore
    .\/ Endpoint "mintTokensStealNft" (MintMore, AssetClass)
    .\/ Endpoint "mintVariousTokens" MintMore
    .\/ Endpoint "addNFTChangeDatum" AddNFT
    .\/ Endpoint "partialReturn" (AssetClass, AssetClass)
    .\/ Endpoint "returnNoNFT" ()
    .\/ Endpoint "returnNFTFraudFrac" AssetClass

endpoints :: Contract () FracNFTEvilSchema Text ()
endpoints =
  forever $
    handleError logError $
      awaitPromise $
        extraFractionNFT' `select` extraFractionNFTWithArbitraryDatumUtxo' `select` mintTokensNoContract' `select` burn' `select` addNFT' `select` mintMoreTokens'
          `select` mintSteal'
          `select` mintVariousTokens'
          `select` addNFTChangeDatum'
          `select` partialReturn'
          `select` returnNoNFT'
          `select` fractionNFTRecordFraudTokensClass'
          `select` mintFraudTokens'
          `select` returnNFTFraudFrac'
  where
    extraFractionNFT' = endpoint @"extraFractionNFT" $ extraFractionNFT
    extraFractionNFTWithArbitraryDatumUtxo' = endpoint @"extraFractionNFTWithArbitraryDatumUtxo" $ extraFractionNFTWithArbitraryDatumUtxo
    mintTokensNoContract' = endpoint @"mintTokensNoContract" $ mintTokensNoContract
    burn' = endpoint @"returnNFTNoFrac" $ returnNFTNoFrac
    addNFT' = endpoint @"stealWhenAdding" $ stealWhenAdding
    mintMoreTokens' = endpoint @"mintExtraTokens" $ mintExtraTokens
    mintSteal' = endpoint @"mintTokensStealNft" $ mintTokensStealNft
    mintVariousTokens' = endpoint @"mintVariousTokens" $ mintVariousTokens
    addNFTChangeDatum' = endpoint @"addNFTChangeDatum" $ addNFTChangeDatum
    partialReturn' = endpoint @"partialReturn" $ partialReturn
    returnNoNFT' = endpoint @"returnNoNFT" $ returnNoNFT
    fractionNFTRecordFraudTokensClass' = endpoint @"fractionNFTRecordFraudTokensClass" $ fractionNFTRecordFraudTokensClass
    mintFraudTokens' = endpoint @"mintFraudTokens" $ mintFraudTokens
    returnNFTFraudFrac' = endpoint @"returnNFTFraudFrac" $ returnNFTFraudFrac

-- reuse signature while adding a different nft
-- reuse signature while minting fract
