{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-local-binds #-}

module Spec.Scenarios where

import           Control.Lens           ((^.))
import           Control.Monad          hiding (fmap)
import           Data.Default           (Default (..))
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Fracada.Offchain       as OC
import           Fracada.Utils
import           Fracada.Minting
import           Ledger                 hiding (singleton)
import           Ledger.Ada             as Ada
import           Ledger.CardanoWallet   as CW
import           Ledger.Value           as Value
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import           Prelude                ((<>), drop)
import           Spec.EvilEndpoints     as Evil
import           Wallet.Emulator.Wallet

nftCurrency :: CurrencySymbol
nftCurrency = "bb7cd5359aa4de1dc9725fb7d8283922185d1cdbfe5fdf35df46c028"

nft :: AssetClass
nft = AssetClass (nftCurrency, "NFT")

nft2 :: AssetClass
nft2 = AssetClass (nftCurrency, "NFT2")

nft3 :: AssetClass
nft3 = AssetClass (nftCurrency, "NFT3")

nftTokens :: [AssetClass]
nftTokens =
  [
    AssetClass (nftCurrency, "NFT4")
  , AssetClass (nftCurrency, "NFT5")
  , AssetClass (nftCurrency, "NFT6")
  , AssetClass (nftCurrency, "NFT7")
  , AssetClass (nftCurrency, "NFT8")
  , AssetClass (nftCurrency, "NFT9")
  , AssetClass (nftCurrency, "NFT10")
  , AssetClass (nftCurrency, "NFT11")
  , AssetClass (nftCurrency, "NFT12")
  , AssetClass (nftCurrency, "NFT13")
  , AssetClass (nftCurrency, "NFT14")
  , AssetClass (nftCurrency, "NFT15")
  , AssetClass (nftCurrency, "NFT16")
  , AssetClass (nftCurrency, "NFT17")
  , AssetClass (nftCurrency, "NFT18")
  , AssetClass (nftCurrency, "NFT19")
  , AssetClass (nftCurrency, "NFT20")
  , AssetClass (nftCurrency, "NFT21")
  , AssetClass (nftCurrency, "NFT22")
  , AssetClass (nftCurrency, "NFT23")
  ]

wallets :: [Wallet]
wallets = [w1, w2, w3, w4, w5]

privKeys :: [PaymentPrivateKey]
privKeys = map (paymentPrivateKey . fromJust' . walletToMockWallet) wallets

pubKeysHashes :: [PubKeyHash]
pubKeysHashes = map unPaymentPubKeyHash (mockWalletPaymentPubKeyHash <$> wallets)

minSigns :: Integer
minSigns = 2

baseValue :: Value
baseValue = Ada.lovelaceValueOf 1_000_000_000
         <> assetClassValue nft 1
         <> assetClassValue nft2 2
         <> assetClassValue nft3 1

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(w1, baseValue), (w2, baseValue), (w3, v3), (w4, v4), (w5, v5)]) def
  where
    v3 = baseValue <> mconcat (map (`assetClassValue` 1) $ take 5 nftTokens) -- put the first half of the tokens to the first wallet
    v4 = baseValue <> mconcat (map (`assetClassValue` 1) $ take 7 $ drop 5 nftTokens) -- and the second hald to the second wallet
    v5 = baseValue <> mconcat (map (`assetClassValue` 1) $ take 7 $ drop 12 nftTokens) -- and the second hald to the second wallet

toFraction :: Integer -> ToFraction
toFraction fractionsCount =
  ToFraction {
    fractions = fractionsCount,
    pubKeys = pubKeysHashes,
    minSigs = minSigns,
    initTokenClass = nft
  }

mintMore :: Integer -> MintMore
mintMore count =
  MintMore {
    mm_count = count,
    mm_sigs = pubKeysHashes
  }

addNFTParams :: AssetClass -> AddNFT
addNFTParams asset =
  AddNFT {
    an_asset = asset,
    an_sigs = pubKeysHashes
  }

unsignedHandle :: Wallet -> EmulatorTrace (ContractHandle () FracNFTSchema Text)
unsignedHandle wallet = do
  activateContractWallet wallet OC.endpoints

signedHandle :: Wallet -> EmulatorTrace (ContractHandle () FracNFTSchema Text)
signedHandle wallet = do
  Emulator.setSigningProcess wallet (Just $ signPrivateKeys privKeys)
  activateContractWallet wallet OC.endpoints

unsignedEvilHandle :: Wallet -> EmulatorTrace (ContractHandle () FracNFTEvilSchema Text)
unsignedEvilHandle wallet = do
  activateContractWallet wallet Evil.endpoints

signedEvilHandle :: Wallet -> EmulatorTrace (ContractHandle () FracNFTEvilSchema Text)
signedEvilHandle wallet = do
  Emulator.setSigningProcess wallet (Just $ signPrivateKeys privKeys)
  activateContractWallet wallet Evil.endpoints

callFractionNFT :: Wallet -> EmulatorTrace ()
callFractionNFT wallet = do
  handle <- unsignedHandle wallet
  callEndpoint @"fractionNFT" handle $ toFraction 10
  void $ Emulator.waitNSlots 1

callReturnNFT :: Wallet -> EmulatorTrace ()
callReturnNFT wallet = do
  handle <- unsignedHandle wallet
  callEndpoint @"returnNFT" handle nft
  void $ Emulator.waitNSlots 1

callAddNFT :: Wallet -> AssetClass -> EmulatorTrace ()
callAddNFT wallet asset = do
  handle <- signedHandle wallet
  callEndpoint @"addNFT" handle $ addNFTParams asset
  void $ Emulator.waitNSlots 1

callMintMoreTokens :: Wallet -> Integer -> EmulatorTrace ()
callMintMoreTokens wallet count = do
  handle <- signedHandle wallet
  callEndpoint @"mintMoreTokens" handle $ mintMore count
  void $ Emulator.waitNSlots 1

-- lock and unlock
scenario1 :: EmulatorTrace ()
scenario1 = do
  callFractionNFT w1
  callReturnNFT w1

-- lock, add tokens, mint, and unlock
scenario2 :: EmulatorTrace ()
scenario2 = do
  callFractionNFT w1
  callAddNFT w1 nft2
  callMintMoreTokens w1 20
  callReturnNFT w1

-- lock, mint, and unlock
scenario3 :: EmulatorTrace ()
scenario3 = do
  callFractionNFT w1
  callMintMoreTokens w1 20
  callReturnNFT w1

-- lock, mint, 2x add tokens, and unlock
scenario4 :: EmulatorTrace ()
scenario4 = do
  callFractionNFT w1
  callMintMoreTokens w1 20
  callAddNFT w1 nft2
  callAddNFT w2 nft3
  callReturnNFT w1

-- lock and unlock
duplicatePubKeys :: EmulatorTrace ()
duplicatePubKeys = do
  handle <- unsignedHandle w1
  callEndpoint @"fractionNFT" handle $ (toFraction 10) {pubKeys = [head pubKeysHashes, head pubKeysHashes]}
  void $ Emulator.waitNSlots 1

initialExtraMint :: EmulatorTrace ()
initialExtraMint = do
  handle <- unsignedEvilHandle w2
  callEndpoint @"extraFractionNFT" handle $ toFraction 10
  void $ Emulator.waitNSlots 1

initialExtraMintWithArbitraryDatumUtxo :: EmulatorTrace ()
initialExtraMintWithArbitraryDatumUtxo = do
  handle <- unsignedEvilHandle w2
  callEndpoint @"extraFractionNFTWithArbitraryDatumUtxo" handle $ toFraction 10
  void $ Emulator.waitNSlots 1

mintingWithoutContract :: EmulatorTrace ()
mintingWithoutContract = do
  h2 <- unsignedEvilHandle w2
  callEndpoint @"mintTokensNoContract" h2 $ toFraction 10
  void $ Emulator.waitNSlots 1

returnNFTNoTotalBurn :: Integer -> EmulatorTrace ()
returnNFTNoTotalBurn toBurn = do
  callFractionNFT w1

  handle <- unsignedEvilHandle w1
  callEndpoint @"returnNFTNoFrac" handle (toBurn, nft)
  void $ Emulator.waitNSlots 1

mintExtraTokens :: EmulatorTrace ()
mintExtraTokens = do
  callFractionNFT w1

  handle <- signedEvilHandle w2
  callEndpoint @"mintExtraTokens" handle $ mintMore 10
  void $ Emulator.waitNSlots 1

-- Lock NFT minting legit fraction tokens but recording fraudulent asset class for the fraction tokens,
-- then mint the fraudulent tokens, and return the NFT while burning the fraudulent tokens.
-- In result you have minted legit fraction tokens for free
maliciousMintingPolicy :: EmulatorTrace ()
maliciousMintingPolicy = do
  handle <- unsignedEvilHandle w1

  callEndpoint @"fractionNFTRecordFraudTokensClass" handle $ toFraction 10
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintFraudTokens" handle $ toFraction 10
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNFTFraudFrac" handle nft
  void $ Emulator.waitNSlots 1

-- lock, add token without signing transaction by minimum signers
unsignedNFT :: EmulatorTrace ()
unsignedNFT = do
  callFractionNFT w1

  handle <- unsignedHandle w1
  callEndpoint @"addNFT" handle $ addNFTParams nft2
  void $ Emulator.waitNSlots 1

-- lock, mint token without signing transaction by minimum signers
unsignedMinting :: EmulatorTrace ()
unsignedMinting = do
  callFractionNFT w1

  handle <- unsignedHandle w1
  callEndpoint @"mintMoreTokens" handle $ mintMore 20
  void $ Emulator.waitNSlots 1

stealWhenAdding :: EmulatorTrace ()
stealWhenAdding = do
  callFractionNFT w1

  handle <- signedEvilHandle w2
  callEndpoint @"stealWhenAdding" handle $ addNFTParams nft
  void $ Emulator.waitNSlots 1

mintAndSteal :: EmulatorTrace ()
mintAndSteal = do
  callFractionNFT w1
  callAddNFT w1 nft3

  handle <- signedEvilHandle w2
  callEndpoint @"mintTokensStealNft" handle (mintMore 20, nft3)
  void $ Emulator.waitNSlots 1

mintExtraneousTokens :: EmulatorTrace ()
mintExtraneousTokens = do
  callFractionNFT w1

  handle <- signedEvilHandle w2
  callEndpoint @"mintVariousTokens" handle $ mintMore 10
  void $ Emulator.waitNSlots 1

repeatToken :: EmulatorTrace ()
repeatToken = do
  callFractionNFT w1
  callAddNFT w1 nft2
  callAddNFT w1 nft2

addNFTChangeDatum :: EmulatorTrace ()
addNFTChangeDatum = do
  callFractionNFT w1

  handle <- signedEvilHandle w2
  callEndpoint @"addNFTChangeDatum" handle $ addNFTParams nft2
  void $ Emulator.waitNSlots 1

dontRemoveAll :: EmulatorTrace ()
dontRemoveAll = do
  callFractionNFT w1
  callAddNFT w1 nft2

  handle <- unsignedEvilHandle w1
  callEndpoint @"partialReturn" handle (nft2, nft)
  void $ Emulator.waitNSlots 1

burnNoNFT :: EmulatorTrace ()
burnNoNFT = do
  callFractionNFT w1

  handle <- unsignedEvilHandle w1
  callEndpoint @"returnNoNFT" handle ()
  void $ Emulator.waitNSlots 1

--lock NFT, add fraction token
addFractionToken :: EmulatorTrace ()
addFractionToken = do
  -- get utxo at the mock wallet address
  state <- Emulator.chainState
  let addr      = mockWalletAddress w1
      utxoIndex = getIndex $ state ^. Emulator.index
      utxos     = [oref | (oref, o) <- Map.toList utxoIndex, txOutAddress o == addr]
      utxo      = head utxos

  callFractionNFT w1

  let fractionTokenName = TokenName {unTokenName = calculateFractionTokenNameHash utxo}
      mintingScript = fracadaPolicy
      currency = scriptCurrencySymbol mintingScript

  callAddNFT w1 $ AssetClass (currency, fractionTokenName)

-- lock and add token many times until the limit is exceeded
transactionLimitExceeded :: EmulatorTrace ()
transactionLimitExceeded = do
  callFractionNFT w3

  -- It's necessary to split addNFT between two wallet, because it's too many tokens for one wallet
  -- and it results in the error OutputTooSmallUTxO
  forM_ ([nft2, nft3] ++ (take 5 nftTokens)) $ \token -> do
    callAddNFT w3 token

  forM_ (take 7 $ drop 5 nftTokens) $ \token -> do
    callAddNFT w4 token

  forM_ (take 7 $ drop 12 nftTokens) $ \token -> do
    callAddNFT w5 token