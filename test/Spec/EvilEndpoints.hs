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

module Spec.EvilEndpoints where

import           Control.Monad        hiding (fmap)
-- import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
-- import           GHC.Generics        (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import           Ledger.Value         as Value
-- import           Playground.Contract (ToSchema)
import           Plutus.Contract      as Contract
-- import qualified PlutusTx
-- import           PlutusTx.Builtins   as Builtins
import           PlutusTx.IsData
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import           Prelude              (Semigroup (..), String, show)
import           Text.Printf          (printf)

import           Fracada.Minting
import           Fracada.Offchain
import           Fracada.Validator
-- import qualified Prelude             as Prelude
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx

mintTokensNoNFT :: FractionNFTParameters -> ToFraction -> Contract w FracNFTEvilSchema Text ()
mintTokensNoNFT params@FractionNFTParameters{initTokenClass} ToFraction {fractions, fractionTokenName} = do
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

      mintRedeemer = Redeemer $ toBuiltinData fractions
      --build the constraints and submit the transaction
      validator = fractionValidatorScript params
      lookups = Constraints.mintingPolicy mintingScript  <>
                Constraints.otherScript validator
      tx      = Constraints.mustMintValueWithRedeemer mintRedeemer tokensToMint  <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s for NFT %s" (show fractions) (show initTokenClass)

returnNFTNoFrac :: FractionNFTParameters -> () -> Contract w FracNFTEvilSchema Text ()
returnNFTNoFrac params@FractionNFTParameters{initTokenClass} _ = do
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
      amountToBurn = 0 --  negate totalFractions
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
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "burnt %s" (show totalFractions)


addMoreNFT :: FractionNFTParameters -> AddNFT -> Contract w FracNFTEvilSchema Text ()
addMoreNFT params AddNFT{an_asset, an_sigs} = do
    utxosAtValidator <- utxosAt ( fractionNftValidatorAddress params)
    let
      -- value of NFT
      valueToScript = (valueOfTxs utxosAtValidator) <> assetClassValue an_asset 2
      nftTx = snd.head $ Map.toList utxosAtValidator

    previousDatum  <- extractData nftTx
    let
        --update datum incrementing the count of nfts
        updatedDatum = previousDatum { newNftClass = an_asset }
        redeemer = Just AddToken { newToken=an_asset,signatures'=an_sigs}
        validatorScript = fractionNftValidatorInstance params
        tx       = collectFromScript utxosAtValidator redeemer <> mustPayToTheScript updatedDatum valueToScript

    void $ submitTxConstraintsSpending validatorScript utxosAtValidator tx
    Contract.logInfo @String $ printf "added new NFT %s" (show an_asset)

mintExtraTokens :: FractionNFTParameters -> MintMore -> Contract w FracNFTEvilSchema Text ()
mintExtraTokens params MintMore{mm_count, mm_sigs} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
    pkh   <- Contract.ownPubKeyHash
    utxosAtValidator <- utxosAt ( fractionNftValidatorAddress params)
    currentDatum@FractionNFTDatum{ tokensClass, totalFractions=currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
    let
      fractionTokenName = snd $ unAssetClass tokensClass
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint =  Value.singleton currency fractionTokenName (mm_count + 1  )
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum{ totalFractions = currentFractions + mm_count}
      mintRedeemer = Redeemer $ toBuiltinData mm_count

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator
      redeemer = Just AddToken { newToken=tokensClass,signatures'=mm_sigs }

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups = Constraints.mintingPolicy mintingScript  <>
                Constraints.unspentOutputs utxosAtValidator <>
                Constraints.typedValidatorLookups validator
      tx      = Constraints.mustMintValueWithRedeemer mintRedeemer tokensToMint  <>
                Constraints.mustPayToTheScript newDatum valueToScript <>
                collectFromScript utxosAtValidator redeemer <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Fractioning lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)







mintTokensStealNft :: FractionNFTParameters -> (MintMore,AssetClass) -> Contract w FracNFTEvilSchema Text ()
mintTokensStealNft params (MintMore{mm_count, mm_sigs}, nftToSteal) = do
  -- pay nft to contract
  -- pay minted tokens back to signer
    pkh   <- Contract.ownPubKeyHash
    utxosAtValidator <- utxosAt ( fractionNftValidatorAddress params)
    currentDatum@FractionNFTDatum{ tokensClass, totalFractions=currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
    let
      fractionTokenName = snd $ unAssetClass tokensClass
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      stealValue = assetClassValue nftToSteal 1
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint =  Value.singleton currency fractionTokenName (mm_count)
      payBackTokens = mustPayToPubKey pkh $ tokensToMint <> stealValue -- 1 that the wallet already has + 1 in the contract

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum{ totalFractions = currentFractions + mm_count}
      mintRedeemer = Redeemer $ toBuiltinData mm_count

      -- preserve NFTs
      valueToScript = unionWith (-) (valueOfTxs utxosAtValidator) (assetClassValue nftToSteal 1)
      redeemer = Just AddToken { newToken=tokensClass,signatures'=mm_sigs }

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups = Constraints.mintingPolicy mintingScript  <>
                Constraints.unspentOutputs utxosAtValidator <>
                Constraints.typedValidatorLookups validator
      tx      = Constraints.mustMintValueWithRedeemer mintRedeemer tokensToMint  <>
                Constraints.mustPayToTheScript newDatum valueToScript <>
                collectFromScript utxosAtValidator redeemer <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Fractioning lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)


{-# INLINABLE anyMintScript #-}
anyMintScript :: () -> ScriptContext -> Bool
anyMintScript _ _ = True



anyMintScriptPolicy :: MintingPolicy
anyMintScriptPolicy  = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy $ anyMintScript||])

anyMintCurSymbol :: CurrencySymbol
anyMintCurSymbol  = scriptCurrencySymbol $ anyMintScriptPolicy

mintVariousTokens :: FractionNFTParameters -> MintMore -> Contract w FracNFTEvilSchema Text ()
mintVariousTokens params MintMore{mm_count, mm_sigs} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
    pkh   <- Contract.ownPubKeyHash
    utxosAtValidator <- utxosAt ( fractionNftValidatorAddress params)
    currentDatum@FractionNFTDatum{ tokensClass, totalFractions=currentFractions} <- extractData $ snd $ head $ Map.toList utxosAtValidator
    let
      fractionTokenName = snd $ unAssetClass tokensClass
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy params fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokensToMint =  Value.singleton currency fractionTokenName mm_count
      extraTokens = Value.singleton anyMintCurSymbol "EXTRA" 10
      payBackTokens = mustPayToPubKey pkh tokensToMint

      -- keep the minted amount and asset class in the datum
      newDatum = currentDatum{ totalFractions = currentFractions + mm_count}
      mintRedeemer = Redeemer $ toBuiltinData mm_count

      -- preserve NFTs
      valueToScript = valueOfTxs utxosAtValidator
      redeemer = Just AddToken { newToken=tokensClass,signatures'=mm_sigs }

      --build the constraints and submit the transaction
      validator = fractionNftValidatorInstance params
      lookups = Constraints.mintingPolicy mintingScript  <>
                Constraints.mintingPolicy anyMintScriptPolicy  <>
                Constraints.unspentOutputs utxosAtValidator <>
                Constraints.typedValidatorLookups validator
      tx      = Constraints.mustMintValueWithRedeemer mintRedeemer tokensToMint  <>
                Constraints.mustMintValueWithRedeemer (Redeemer $ toBuiltinData () ) extraTokens  <>
                Constraints.mustPayToTheScript newDatum valueToScript <>
                collectFromScript utxosAtValidator redeemer <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Fractioning lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s extra tokens, total %s " (show mm_count) (show $ currentFractions + mm_count)



type FracNFTEvilSchema = Endpoint "mintTokensNoNFT" ToFraction
    .\/ Endpoint "returnNFTNoFrac" ()
    .\/ Endpoint "addMoreNFT" AddNFT
    .\/ Endpoint "mintExtraTokens" MintMore
    .\/ Endpoint "mintTokensStealNft" (MintMore, AssetClass)
    .\/ Endpoint "mintVariousTokens" MintMore

endpoints :: FractionNFTParameters ->  Contract () FracNFTEvilSchema Text ()
endpoints params = forever
                $ handleError logError
                $ awaitPromise
                $ mintTokensNoNFT' `select` burn' `select` addNFT' `select` mintMoreTokens' `select` mintSteal' `select` mintVariousTokens'
          where
            mintTokensNoNFT' = endpoint @"mintTokensNoNFT" $ mintTokensNoNFT params
            burn' = endpoint @"returnNFTNoFrac" $ returnNFTNoFrac params
            addNFT' = endpoint @"addMoreNFT" $ addMoreNFT params
            mintMoreTokens' = endpoint @"mintExtraTokens" $ mintExtraTokens params
            mintSteal' = endpoint @"mintTokensStealNft" $ mintTokensStealNft params
            mintVariousTokens' = endpoint @"mintVariousTokens" $ mintVariousTokens params
