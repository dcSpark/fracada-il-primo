{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Fracada (
    useCaseTests
    ) where

import           Control.Lens
import           Control.Monad          hiding (fmap)
import qualified Fracada.Offchain       as Fracada
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator  as Trace
import qualified Spec.Scenarios         as E
import           Test.Tasty
import           Wallet.Emulator.Wallet

useCaseTests :: TestTree
useCaseTests =
    let
        contract = Fracada.endpoints E.contractParams
        options = defaultCheckOptions & emulatorConfig .~ E.emCfg
    in
        testGroup "fracada"
        [ checkPredicate "Expose endpoints"
        (endpointAvailable @"fractionNFT" contract (Trace.walletInstanceTag $ toMockWallet E.w1)
        .&&. endpointAvailable @"returnNFT" contract (Trace.walletInstanceTag $ toMockWallet  E.w1)
        .&&. endpointAvailable @"addNFT" contract (Trace.walletInstanceTag $ toMockWallet  E.w1)
        .&&. endpointAvailable @"mintMoreTokens" contract (Trace.walletInstanceTag $ toMockWallet  E.w1)
        ) $ void (Trace.activateContractWallet (toMockWallet E.w1) contract)
        , checkPredicateOptions options "Can lock NFT, mint fractional tokens, and exchange the NFT back when burning the tokens" assertNoFailedTransactions E.scenario1
        , checkPredicateOptions options "Full scenario (lock NFT with minting, add more NFTs, mint more tokens, return all NFTs in echange of tokens" assertNoFailedTransactions E.scenario2

         ]
