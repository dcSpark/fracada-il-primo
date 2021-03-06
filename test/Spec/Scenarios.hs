{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-local-binds #-}

module Spec.Scenarios where

import           Control.Monad          hiding (fmap)
import           Data.Default           (Default (..))
import qualified Data.Map               as Map
import           Fracada.Minting
import           Fracada.Offchain       as OC
import           Fracada.Utils
import           Fracada.Validator
import           Ledger                 hiding (singleton)
import           Ledger.Ada             as Ada
import           Ledger.CardanoWallet   as CW
import qualified Ledger.Crypto          as Crypto
import           Ledger.Value           as Value
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.IsData
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import           Prelude                ((<>))
import           Spec.EvilEndpoints     as Evil
import           Wallet.Emulator.Wallet

nftCurrency :: CurrencySymbol
nftCurrency = "66"

nftName :: TokenName
nftName = "NFT"

nft :: AssetClass
nft = AssetClass (nftCurrency, nftName)

nft2 :: AssetClass
nft2 = assetClass "bb7cd5359aa4de1dc9725fb7d8283922185d1cdbfe5fdf35df46c028" "NFT2"

nft3 :: AssetClass
nft3 = assetClass "bb7cd5359aa4de1dc9725fb7d8283922185d1cdbfe5fdf35df46c028" "NFT3"

passphrase :: Passphrase
passphrase = ""

wallets :: [Wallet]
wallets = [w1, w2, w3]

privKeys :: [PaymentPrivateKey]
privKeys = map (paymentPrivateKey . fromJust' . walletToMockWallet) wallets

minSigs :: Integer
minSigs = 2

contractParams :: FractionNFTParameters
contractParams =
  FractionNFTParameters
    { initTokenClass = nft,
      authorizedPubKeys = map (unPaymentPubKey . mockWalletPaymentPubKey) [w1, w2, w3],
      minSigRequired = minSigs
    }

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(w, v) | w <- [w1, w2, w3]]) def def
  where
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue nft 1 <> assetClassValue nft2 2 <> assetClassValue nft3 1

signDatumHash :: FractionNFTDatum -> [Signature]
signDatumHash fracDatum = map signMsg privKeys
  where
    signMsg privKey = Crypto.sign msgHash (unPaymentPrivateKey privKey) passphrase
    datum' = Datum $ toBuiltinData fracDatum
    DatumHash msgHash = datumHash datum'

-- lock and unlock
scenario1 :: EmulatorTrace ()
scenario1 = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let toFraction = ToFraction {fractions = 10, fractionTokenName = tokenName "Frac"}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNFT" h1 ()
  void $ Emulator.waitNSlots 1

-- lock, add tokens, mint, and unlock
scenario2 :: EmulatorTrace ()
scenario2 = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtAdd = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 10, newNftClass = nft2}
      newToken = AddNFT {an_asset = nft2, an_sigs = signDatumHash expectedDatumAtAdd}

      expectedDatumAtMint = expectedDatumAtAdd {totalFractions = 30, newNftClass = tokenClass}
      mintMore = MintMore {mm_count = 20, mm_sigs = signDatumHash expectedDatumAtMint}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintMoreTokens" h1 mintMore
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNFT" h1 ()

--no new NFT
scenario3 :: EmulatorTrace ()
scenario3 = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtMint = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 30, newNftClass = tokenClass}
      mintMore = MintMore {mm_count = 20, mm_sigs = signDatumHash expectedDatumAtMint}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintMoreTokens" h1 mintMore
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNFT" h1 ()

-- lock,  mint, add tokens,, and unlock
scenario4 :: EmulatorTrace ()
scenario4 = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtMint = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 30, newNftClass = tokenClass}
      mintMore = MintMore {mm_count = 20, mm_sigs = signDatumHash expectedDatumAtMint}

      expectedDatumAtAdd = expectedDatumAtMint {newNftClass = nft2}
      newToken = AddNFT {an_asset = nft2, an_sigs = signDatumHash expectedDatumAtAdd}

      expectedDatumAtAdd1 = expectedDatumAtMint {newNftClass = nft3}
      newToken1 = AddNFT {an_asset = nft3, an_sigs = signDatumHash expectedDatumAtAdd1}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintMoreTokens" h1 mintMore
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken1
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNFT" h1 ()

initialExtraMint :: EmulatorTrace ()
initialExtraMint = do
  h2 <- activateContractWallet w2 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}

  callEndpoint @"extraFractionNFT" h2 toFraction
  void $ Emulator.waitNSlots 1

notLocked :: EmulatorTrace ()
notLocked = do
  h2 <- activateContractWallet w2 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}

  callEndpoint @"mintTokensNoNFT" h2 toFraction
  void $ Emulator.waitNSlots 1

returnNFTNoTotalBurn :: Integer -> EmulatorTrace ()
returnNFTNoTotalBurn toBurn = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w1 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNFTNoFrac" h2 toBurn
  void $ Emulator.waitNSlots 1

mintExtraTokens :: EmulatorTrace ()
mintExtraTokens = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w2 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtMint = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 20, newNftClass = tokenClass}
      mintMore = MintMore {mm_count = 10, mm_sigs = signDatumHash expectedDatumAtMint}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintExtraTokens" h2 mintMore

-- lock, add unsigned token
unsignedNFT :: EmulatorTrace ()
unsignedNFT = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtAdd = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 10, newNftClass = nft2}
      newToken = AddNFT {an_asset = nft3, an_sigs = signDatumHash expectedDatumAtAdd}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken
  void $ Emulator.waitNSlots 1

-- lock, mint different amount of signed token
unsignedMinting :: EmulatorTrace ()
unsignedMinting = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtMint = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 15, newNftClass = tokenClass}
      mintMore = MintMore {mm_count = 20, mm_sigs = signDatumHash expectedDatumAtMint}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintMoreTokens" h1 mintMore
  void $ Emulator.waitNSlots 1

addExtraToken :: EmulatorTrace ()
addExtraToken = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w2 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}

      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtAdd = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 10, newNftClass = nft2}
      newToken = AddNFT {an_asset = nft2, an_sigs = signDatumHash expectedDatumAtAdd}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"addMoreNFT" h2 newToken
  void $ Emulator.waitNSlots 1

mintAndSteal :: EmulatorTrace ()
mintAndSteal = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w1 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtAdd = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 10, newNftClass = nft3}
      newToken = AddNFT {an_asset = nft3, an_sigs = signDatumHash expectedDatumAtAdd}

      expectedDatumAtMint = expectedDatumAtAdd {totalFractions = 30}
      mintMore = MintMore {mm_count = 20, mm_sigs = signDatumHash expectedDatumAtMint}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintTokensStealNft" h2 (mintMore, nft3)

mintExtraneousTokens :: EmulatorTrace ()
mintExtraneousTokens = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w1 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtMint = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 20, newNftClass = tokenClass}
      mintMore = MintMore {mm_count = 10, mm_sigs = signDatumHash expectedDatumAtMint}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"mintVariousTokens" h2 mintMore
  void $ Emulator.waitNSlots 1

repeatToken :: EmulatorTrace ()
repeatToken = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtAdd = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 10, newNftClass = nft2}
      newToken = AddNFT {an_asset = nft2, an_sigs = signDatumHash expectedDatumAtAdd}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken
  void $ Emulator.waitNSlots 1

addNFTNoUpdate :: EmulatorTrace ()
addNFTNoUpdate = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w1 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtAdd = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 10, newNftClass = tokenClass}
      newToken = AddNFT {an_asset = nft2, an_sigs = signDatumHash expectedDatumAtAdd}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFTNoDatumUpd" h2 newToken
  void $ Emulator.waitNSlots 1

dontRemoveAll :: EmulatorTrace ()
dontRemoveAll = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w1 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let tknName = tokenName "Frac"
      toFraction = ToFraction {fractions = 10, fractionTokenName = tknName}
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy contractParams tknName
      -- define the value to mint (amount of tokens) and be paid to signer
      currency = scriptCurrencySymbol mintingScript
      tokenClass = assetClass currency tknName

      expectedDatumAtAdd = FractionNFTDatum {tokensClass = tokenClass, totalFractions = 10, newNftClass = nft2}
      newToken = AddNFT {an_asset = nft2, an_sigs = signDatumHash expectedDatumAtAdd}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"addNFT" h1 newToken
  void $ Emulator.waitNSlots 1

  callEndpoint @"partialReturn" h2 ()

burnNoNFT :: EmulatorTrace ()
burnNoNFT = do
  h1 <- activateContractWallet w1 $ OC.endpoints contractParams
  h2 <- activateContractWallet w1 $ Evil.endpoints contractParams
  void $ Emulator.waitNSlots 1
  let toFraction = ToFraction {fractions = 10, fractionTokenName = tokenName "Frac"}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNoNFT" h2 ()
  void $ Emulator.waitNSlots 1
