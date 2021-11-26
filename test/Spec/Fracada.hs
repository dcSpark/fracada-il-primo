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
import           Ledger.Index           (ValidationError (ScriptFailure))
import           Ledger.Scripts         (ScriptError (EvaluationError))
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
        , checkPredicateOptions options "No new NFTs (lock NFT with minting, mint more tokens, return all NFTs in echange of tokens" assertNoFailedTransactions E.scenario3
        , checkPredicateOptions options "Full scenario (lock NFT with minting, mint more tokens, add more NFTs, return all NFTs in echange of tokens" assertNoFailedTransactions E.scenario4
        -- validation error scenarios
        , checkPredicateOptions options "Can't return the nft if fractional tokens aren't burned" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Tokens not burned", "PT5"] _) -> True; _ -> False  })) $ E.returnNFTNoTotalBurn 0
        , checkPredicateOptions options "Can't return the nft if not all fractional tokens aren't burned" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Tokens not burned", "PT5"] _) -> True; _ -> False  })) $ E.returnNFTNoTotalBurn 5
        , checkPredicateOptions options "Can't mint fractional tokens" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["datum not updated forging tokens", "PT5"] _) -> True; _ -> False  })) E.mintExtraTokens
        , checkPredicateOptions options "Can't add unsigned nft" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["not enough signatures to add tokens", "PT5"] _) -> True; _ -> False  })) E.unsignedNFT
        , checkPredicateOptions options "Can't mint unsigned tokens" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["not enough signatures for minting", "PT5"] _) -> True; _ -> False  })) E.unsignedMinting
        , checkPredicateOptions options "Can't add more than one token" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Value change not +1 NFT", "PT5"] _) -> True; _ -> False  })) E.addExtraToken
        , checkPredicateOptions options "Can't steal NFTs when minting" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["contract value not preserved", "PT5"] _) -> True; _ -> False  })) E.mintAndSteal
        , checkPredicateOptions options "Can't mint extraneous tokens" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Unexpected minted amount", "PT5"] _) -> True; _ -> False  })) E.mintExtraneousTokens
        , checkPredicateOptions options "Can't add the same NFT" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Token already added", "PT5"] _) -> True; _ -> False  })) E.repeatToken
        , checkPredicateOptions options "datum not updated adding NFTs" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["datum not updated adding NFTs", "PT5"] _) -> True; _ -> False  })) E.addNFTNoUpdate
        , checkPredicateOptions options "Must return all NFTs" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["not all NFTs returned", "PT5"] _) -> True; _ -> False  })) E.dontRemoveAll
        -- minting error scenarios
        , checkPredicateOptions options "Can't mint if not locked" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Asset not locked", "PT5"] _) -> True; _ -> False  })) E.notLocked
        , checkPredicateOptions options "Can't burn if NFTs not returned" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Asset not returned", "PT5"] _) -> True; _ -> False  })) E.burnNoNFT
         ]
