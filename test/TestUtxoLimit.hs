{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestUtxoLimit where

import           Control.Monad
import           Prelude
import           Text.Printf            (printf)

import           Ledger                 hiding (singleton)
import           Ledger.Value           as Value
import           Plutus.Test.Model      as M
import           Plutus.V1.Ledger.Api
import           Test.Tasty

import           Fracada.Constants      as C
import           Fracada.Minting
import           Fracada.Offchain       as OC
import           Fracada.Validator
import           Fracada.Utils

-- How many times to try adding token in the test
-- (make sure there is enough tokens in the nftTokens list, it must be addTokenRepetitions + 1)
addTokenRepetitions :: Int
addTokenRepetitions = 13

main :: IO ()
main = do
  cfg <- readDefaultBchConfig
  defaultMain $ do
    testGroup "Tests"
      [ good cfg "Fraction, add to bag N times and return" test1
      ]
  where
    good :: BchConfig -> String -> Run a -> TestTree
    good cfg = testNoErrorsTrace initFunds cfg

nftCurrency :: CurrencySymbol
nftCurrency = "bb7cd5359aa4de1dc9725fb7d8283922185d1cdbfe5fdf35df46c028"

nftTokens :: [AssetClass]
nftTokens =
  [
    AssetClass (nftCurrency, "TOKEN1")
  , AssetClass (nftCurrency, "TOKEN2")
  , AssetClass (nftCurrency, "TOKEN3")
  , AssetClass (nftCurrency, "TOKEN4")
  , AssetClass (nftCurrency, "TOKEN5")
  , AssetClass (nftCurrency, "TOKEN6")
  , AssetClass (nftCurrency, "TOKEN7")
  , AssetClass (nftCurrency, "TOKEN8")
  , AssetClass (nftCurrency, "TOKEN9")
  , AssetClass (nftCurrency, "TOKEN10")
  , AssetClass (nftCurrency, "TOKEN11")
  , AssetClass (nftCurrency, "TOKEN12")
  , AssetClass (nftCurrency, "TOKEN13")
  , AssetClass (nftCurrency, "TOKEN14")
  , AssetClass (nftCurrency, "TOKEN15")
  ]

adaValue :: Integer -> Value
adaValue = singleton adaSymbol adaToken

initFunds :: Value
initFunds = adaValue 1_000 <> mconcat (map (`assetClassValue` 1) nftTokens)

utxoValue :: [(TxOutRef, TxOut)] -> Value
utxoValue utxos = Prelude.foldl1 (Prelude.<>) $ map (txOutValue . snd) utxos

fractionNFT :: Value -> ToFraction -> PubKeyHash -> Run ()
fractionNFT nft ToFraction {fractions, pubKeys, minSigs} pkh = do
  logInfo $ printf "fractionNFT start"

  futxos <- utxoAt (pubKeyHashAddress (PaymentPubKeyHash pkh) Nothing)
  let utxo = fst $ head futxos

  let validatorScript = fracadaValidatorInstance

      -- tokens to mint
      fractionTokenName = TokenName {unTokenName = calculateFractionTokenNameHash utxo}
      mintingScript = fracadaPolicy
      currency = scriptCurrencySymbol mintingScript
      fractionTokensToMint = Value.singleton currency fractionTokenName fractions
      validityTokenToMint = Value.singleton currency C.fracadaValidityTokenName 1

      fractionAsset = assetClass currency fractionTokenName
      datum = FracadaDatum {fractionAC = fractionAsset, emittedFractions = fractions, authorizedPubKeys = pubKeys, minSigRequired = minSigs}

  sp <- spend pkh $ nft <> adaValue 6
  let tx = addMintRedeemer mintingScript (OC.initialMintRedeemer utxo) $
           mintValue mintingScript (fractionTokensToMint <> validityTokenToMint)
        <> payToScript validatorScript datum (nft <> validityTokenToMint <> adaValue 3)
        <> payToPubKey pkh (fractionTokensToMint <> adaValue 3)
        <> userSpend sp

  M.submitTx pkh tx

addNFT :: Value -> PubKeyHash -> Run ()
addNFT nft pkh = do
  utxosAtValidator <- utxoAt fracadaValidatorAddress
  logInfo $ printf "addNFT: utxosAtValidator: %s" (show utxosAtValidator)

  let utxo = fst $ head utxosAtValidator
  dat <- datumAt @FracadaDatum utxo
  case dat of
    Just datum -> do
      logInfo $ printf "addNFT: datum: %s" (show datum)
      let validatorScript = fracadaValidatorInstance

          scriptValue = utxoValue utxosAtValidator

      sp <- spend pkh nft
      let tx = spendScript validatorScript utxo () datum
            <> payToScript validatorScript datum (scriptValue <> nft)
            <> userSpend sp

      M.submitTx pkh tx

    Nothing -> logError "Error getting datum"

returnNFT :: PubKeyHash -> Run ()
returnNFT pkh = do
  utxosAtValidator <- utxoAt fracadaValidatorAddress
  logInfo $ printf "returnNFT: utxosAtValidator: %s" (show utxosAtValidator)

  let utxo = fst $ head utxosAtValidator
  dat <- datumAt @FracadaDatum utxo
  case dat of
    Just previousDatum@FracadaDatum {fractionAC, emittedFractions} -> do
      logInfo $ printf "returnNFT: previousDatum: %s" (show previousDatum)

      let validatorScript = fracadaValidatorInstance

          -- fraction tokens in the wallet
          fractionTokens = Value.singleton currency fractionTokenName emittedFractions

          -- tokens to burn
          fractionTokenName = snd $ unAssetClass fractionAC
          mintingScript = fracadaPolicy
          currency = curSymbol
          fractionTokensToBurn = Value.singleton currency fractionTokenName (negate emittedFractions)
          validityTokenToBurn = Value.singleton currency C.fracadaValidityTokenName (-1)

          scriptValue = utxoValue utxosAtValidator

      sp <- spend pkh fractionTokens
      let tx = addMintRedeemer mintingScript OC.burnRedeemer $
               spendScript validatorScript utxo () previousDatum
            <> mintValue mintingScript (fractionTokensToBurn <> validityTokenToBurn)
            <> payToPubKey pkh (scriptValue <> validityTokenToBurn)
            <> userSpend sp

      M.submitTx pkh tx

    Nothing -> logError "Error getting datum"

test1 :: Run Bool
test1 = do
  userPkh <- newUser initFunds
  let users = [userPkh]
      toFraction = ToFraction {fractions = 100, pubKeys = users, minSigs = 1, initTokenClass = head nftTokens}
      nftToFraction = assetClassValue (initTokenClass toFraction) 1
  TestUtxoLimit.fractionNFT nftToFraction toFraction userPkh
  M.waitNSlots 1

  forM_ (take addTokenRepetitions $ tail nftTokens) $ \token_asset -> do
    let token = assetClassValue token_asset 1
    TestUtxoLimit.addNFT token userPkh
    M.waitNSlots 1

  TestUtxoLimit.returnNFT userPkh

  noErrors
