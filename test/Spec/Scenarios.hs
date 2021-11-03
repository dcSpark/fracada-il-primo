{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Spec.Scenarios where

import           Control.Monad          hiding (fmap)
import           Data.Default           (Default (..))
import qualified Data.Map               as Map
import           Fracada.Minting
import           Fracada.Offchain       as OC
import           Fracada.Validator
import           Ledger                 hiding (singleton)
import           Ledger.Ada             as Ada
import           Ledger.CardanoWallet   as CW
import           Ledger.Value           as Value
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

w1:: MockWallet
w1 = CW.knownWallet 1
w2:: MockWallet
w2 = CW.knownWallet 2
w3:: MockWallet
w3 = CW.knownWallet 3

wallets :: [MockWallet]
wallets = [w1,w2,w3]

privKeys :: [PrivateKey]
privKeys = map CW.privateKey wallets

minSigs :: Integer
minSigs = 2

contractParams :: FractionNFTParameters
contractParams =  FractionNFTParameters {
      initTokenClass  = nft,
      authorizedPubKeys = map CW.pubKey [w1,w2,w3],
      minSigRequired   = minSigs
    }


emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(toMockWallet w, v) | w <- [w1, w2, w3]]) def def
    where
        v = Ada.lovelaceValueOf 1000_000_000 <> assetClassValue nft 1  <> assetClassValue nft2 2 <> assetClassValue nft3 1

signDatum :: FractionNFTDatum -> [Signature]
signDatum fracDatum = map (sign msgHash) privKeys
                        where
                            datum' = Datum $ toBuiltinData fracDatum
                            DatumHash msgHash = datumHash datum'

-- lock and unlock
scenario1 :: EmulatorTrace ()
scenario1 = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let
        toFraction = ToFraction { fractions = 10, fractionTokenName = tokenName "Frac" }

    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"returnNFT" h1 ()
    void $ Emulator.waitNSlots 1

-- lock, add tokens, and unlock
scenario2 :: EmulatorTrace ()
scenario2 = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }
         --find the minting script instance
        mintingScript = mintFractionTokensPolicy contractParams tknName
        -- define the value to mint (amount of tokens) and be paid to signer
        currency = scriptCurrencySymbol mintingScript
        tokenClass = assetClass currency tknName

        expectedDatumAtAdd = FractionNFTDatum{ tokensClass= tokenClass, totalFractions = 10, newNftClass=nft2}
        newToken = AddNFT { an_asset= nft2, an_sigs= signDatum expectedDatumAtAdd}

        expectedDatumAtMint = expectedDatumAtAdd { totalFractions = 30 }
        mintMore = MintMore { mm_count= 20, mm_sigs= signDatum expectedDatumAtMint }

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
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }
         --find the minting script instance
        mintingScript = mintFractionTokensPolicy contractParams tknName
        -- define the value to mint (amount of tokens) and be paid to signer
        currency = scriptCurrencySymbol mintingScript
        tokenClass = assetClass currency tknName

        expectedDatumAtMint = FractionNFTDatum{ tokensClass= tokenClass, totalFractions = 20, newNftClass=tokenClass}
        mintMore = MintMore { mm_count= 20, mm_sigs= signDatum expectedDatumAtMint }

    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"mintMoreTokens" h1 mintMore
    void $ Emulator.waitNSlots 1

    callEndpoint @"returnNFT" h1 ()


notLocked :: EmulatorTrace ()
notLocked = do
    h2 <- activateContractWallet (toMockWallet w2) $ Evil.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let


        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }

    callEndpoint @"mintTokensNoNFT" h2 toFraction
    void $ Emulator.waitNSlots 1

returnNFTNoFrac :: EmulatorTrace ()
returnNFTNoFrac = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    h2 <- activateContractWallet (toMockWallet w2) $ Evil.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }


    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"returnNFTNoFrac" h2 ()
    void $ Emulator.waitNSlots 1

mintExtraTokens :: EmulatorTrace ()
mintExtraTokens = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    h2 <- activateContractWallet (toMockWallet w2) $ Evil.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }
         --find the minting script instance
        mintingScript = mintFractionTokensPolicy contractParams tknName
        -- define the value to mint (amount of tokens) and be paid to signer
        currency = scriptCurrencySymbol mintingScript
        tokenClass = assetClass currency tknName

        expectedDatumAtMint = FractionNFTDatum{ tokensClass= tokenClass, totalFractions = 20, newNftClass=tokenClass}
        mintMore = MintMore { mm_count= 10, mm_sigs= signDatum expectedDatumAtMint }

    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"mintExtraTokens" h2 mintMore

-- lock, add unsigned token
unsignedNFT :: EmulatorTrace ()
unsignedNFT = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }
         --find the minting script instance
        mintingScript = mintFractionTokensPolicy contractParams tknName
        -- define the value to mint (amount of tokens) and be paid to signer
        currency = scriptCurrencySymbol mintingScript
        tokenClass = assetClass currency tknName

        expectedDatumAtAdd = FractionNFTDatum{ tokensClass= tokenClass, totalFractions = 10, newNftClass=nft2}
        newToken = AddNFT { an_asset= nft3, an_sigs= signDatum expectedDatumAtAdd}

    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"addNFT" h1 newToken
    void $ Emulator.waitNSlots 1

-- lock, mint different amount of signed token
unsignedMinting :: EmulatorTrace ()
unsignedMinting = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }
         --find the minting script instance
        mintingScript = mintFractionTokensPolicy contractParams tknName
        -- define the value to mint (amount of tokens) and be paid to signer
        currency = scriptCurrencySymbol mintingScript
        tokenClass = assetClass currency tknName

        expectedDatumAtMint = FractionNFTDatum{ tokensClass= tokenClass, totalFractions = 15, newNftClass=tokenClass}
        mintMore = MintMore { mm_count= 20, mm_sigs= signDatum expectedDatumAtMint }

    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"mintMoreTokens" h1 mintMore
    void $ Emulator.waitNSlots 1

addExtraToken :: EmulatorTrace ()
addExtraToken = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    h2 <- activateContractWallet (toMockWallet w2) $ Evil.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }

         --find the minting script instance
        mintingScript = mintFractionTokensPolicy contractParams tknName
        -- define the value to mint (amount of tokens) and be paid to signer
        currency = scriptCurrencySymbol mintingScript
        tokenClass = assetClass currency tknName

        expectedDatumAtAdd = FractionNFTDatum{ tokensClass= tokenClass, totalFractions = 10, newNftClass=nft2}
        newToken = AddNFT { an_asset= nft2, an_sigs= signDatum expectedDatumAtAdd}

    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"addMoreNFT" h2 newToken
    void $ Emulator.waitNSlots 1

mintAndSteal :: EmulatorTrace ()
mintAndSteal = do
    h1 <- activateContractWallet (toMockWallet w1) $ OC.endpoints contractParams
    h2 <- activateContractWallet (toMockWallet w2) $ Evil.endpoints contractParams
    void $ Emulator.waitNSlots 1
    let

        tknName = tokenName "Frac"
        toFraction = ToFraction { fractions = 10, fractionTokenName = tknName }
         --find the minting script instance
        mintingScript = mintFractionTokensPolicy contractParams tknName
        -- define the value to mint (amount of tokens) and be paid to signer
        currency = scriptCurrencySymbol mintingScript
        tokenClass = assetClass currency tknName

        expectedDatumAtAdd = FractionNFTDatum{ tokensClass= tokenClass, totalFractions = 10, newNftClass=nft2}
        newToken = AddNFT { an_asset= nft2, an_sigs= signDatum expectedDatumAtAdd}

        expectedDatumAtMint = expectedDatumAtAdd { totalFractions = 30 }
        mintMore = MintMore { mm_count= 20, mm_sigs= signDatum expectedDatumAtMint }


    callEndpoint @"fractionNFT" h1 toFraction
    void $ Emulator.waitNSlots 1

    callEndpoint @"addNFT" h1 newToken
    void $ Emulator.waitNSlots 1

    callEndpoint @"mintTokensStealNft" h2 (mintMore, nft2)
