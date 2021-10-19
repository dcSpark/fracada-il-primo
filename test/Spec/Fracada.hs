{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Fracada (
    useCaseTests
    , successFullFractionalizationTrace
    ) where

import           Control.Monad          hiding (fmap)
import qualified Ledger
import           Ledger.Value           as Value
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator  as Trace
import           Test.Tasty

import qualified Fracada.Offchain       as Fracada
import           Fracada.Validator
import           Ledger.CardanoWallet   as CW
import           Wallet.Emulator.Wallet

-- Contracts' parameters

-- for EmulatorTrace it must be a 28-byte length ByteString
nftSymbol :: Ledger.CurrencySymbol
nftSymbol = currencySymbol "0123456789012345678901234567"

nftName :: Ledger.TokenName
nftName = TokenName "fractionNFT"

nftAssetClass :: Ledger.AssetClass
nftAssetClass = Value.assetClass nftSymbol nftName

fractionTokenName :: Ledger.TokenName
fractionTokenName  = "fractionToken"

fractions :: Integer
fractions = 100

wallet1:: MockWallet
wallet1 = CW.knownWallet 1
wallet2:: MockWallet
wallet2 = CW.knownWallet 2
wallet3:: MockWallet
wallet3 = CW.knownWallet 3

pks :: [Ledger.PubKey]
pks = map CW.pubKey [wallet1,wallet2,wallet3]

minSigs :: Integer
minSigs = 2

contractParams :: FractionNFTParameters
contractParams =  FractionNFTParameters {
      initTokenClass  = nftAssetClass,
      authorizedPubKeys = pks,
      minSigRequired   = minSigs
    }

useCaseTests :: TestTree
useCaseTests =
    let contract = Fracada.endpoints contractParams in
        testGroup "fracada"
        [ checkPredicate "Expose '1-fractionNFT' and '2-returnNFT' endpoints"
        (endpointAvailable @"1-fractionNFT" contract (Trace.walletInstanceTag $ toMockWallet wallet1)
        .&&. endpointAvailable @"2-returnNFT" contract (Trace.walletInstanceTag $ toMockWallet  wallet2)
        ) $ void (Trace.activateContractWallet w1 contract)

        , checkPredicate "Can lock NFT and mint fractional tokens"
        assertNoFailedTransactions
        successFullFractionalizationTrace

        ]

successFullFractionalizationTrace :: Trace.EmulatorTrace ()
successFullFractionalizationTrace = do
    h1 <- Trace.activateContractWallet (w1) $ Fracada.endpoints contractParams
    -- h2 <- Trace.activateContractWallet (knownWallet 2) Fracada.endpoints

    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"1-fractionNFT" h1 Fracada.ToFraction { Fracada.fractions = fractions
                                                              , Fracada.fractionTokenName = fractionTokenName }
    void $ Trace.waitNSlots 1

    -- Trace.callEndpoint @"2-returnNFT" h2 nftAssetClass
    -- void $ Trace.waitNSlots 1
