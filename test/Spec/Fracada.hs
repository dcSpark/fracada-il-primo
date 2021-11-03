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
        , checkPredicateOptions options "No new NFTs (lock NFT with minting, mint more tokens, return all NFTs in echange of tokens" assertNoFailedTransactions E.scenario2
        -- validation error scenarios
        , checkPredicateOptions options "Can't mint if not locked" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Asset not locked", "PT5"] _) -> True; _ -> False  })) E.notLocked
        , checkPredicateOptions options "Can't return the nft if fractional tokens aren't burned" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Tokens not burned", "PT5"] _) -> True; _ -> False  })) E.returnNFTNoFrac
        , checkPredicateOptions options "Can't mint fractional tokens" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["datum not updated forging tokens", "PT5"] _) -> True; _ -> False  })) E.mintExtraTokens
        , checkPredicateOptions options "Can't add unsigned nft" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["not enough signatures to add tokens", "PT5"] _) -> True; _ -> False  })) E.unsignedNFT
        , checkPredicateOptions options "Can't mint unsigned tokens" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["not enough signatures for minting", "PT5"] _) -> True; _ -> False  })) E.unsignedMinting
        , checkPredicateOptions options "Can't add more than one token" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Tokens not added", "PT5"] _) -> True; _ -> False  })) E.addExtraToken
        , checkPredicateOptions options "Can't steal NFTs when minting" ( assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["Tokens not added", "PT5"] _) -> True; _ -> False  })) E.mintAndSteal

         ]
{-
Pending
              traceIfFalse "contract value not preserved"  valuePreserved  &&
              traceIfFalse "Unexpected minted amount" (txInfoMint txInfo == expectedMinted)

            && traceIfFalse "no new value " (not $ isZero $ valueProduced txInfo )
            && traceIfFalse "Token already added" tokenNotPresent
            && traceIfFalse "token preserved " (not $ isZero $ valueInContract )
            && traceIfFalse "datum not updated" datumUpdated
            && traceIfFalse "Unexpected token" valueIncreaseMatchToken

          traceIfFalse "NFT not returned" assetIsReturned &&
          traceIfFalse "Value locked in contract" ( isZero valueInContract ) &&

        traceIfFalse "wrong fraction tokens minted" ( mintedAmount == numberOfFractions)
        traceIfFalse "Asset not returned" assetIsReturned           &&
        traceIfFalse "wrong fraction tokens burned" ( mintedAmount == numberOfFractions)
 -}
