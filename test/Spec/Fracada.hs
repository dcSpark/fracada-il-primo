{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Fracada
  ( useCaseTests,
  )
where

import           Control.Lens
import           Control.Monad         hiding (fmap)
import qualified Fracada.Offchain      as Fracada
import           Ledger.Index          (ValidationError (ScriptFailure))
import           Ledger.Scripts        (ScriptError (EvaluationError))
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import qualified Spec.Scenarios        as E
import           Test.Tasty

useCaseTests :: TestTree
useCaseTests =
  let contract = Fracada.endpoints
      options = defaultCheckOptions & emulatorConfig .~ E.emCfg

      tag :: Trace.ContractInstanceTag
      tag = "instance 1"
   in testGroup
        "fracada"
        [ checkPredicate
            "Expose endpoints"
            ( endpointAvailable @"fractionNFT" contract tag
                .&&. endpointAvailable @"returnNFT" contract tag
                .&&. endpointAvailable @"addNFT" contract tag
                .&&. endpointAvailable @"mintMoreTokens" contract tag
            )
            $ void (Trace.activateContractWallet w1 contract),
            checkPredicateOptions options "Can lock NFT, mint fractional tokens, and exchange the NFT back when burning the tokens" assertNoFailedTransactions E.scenario1,
            checkPredicateOptions options "Full scenario (lock NFT with minting, add more NFTs, mint more tokens, return all NFTs in exchange of tokens" assertNoFailedTransactions E.scenario2,
            checkPredicateOptions options "No new NFTs (lock NFT with minting, mint more tokens, return all NFTs in exchange of tokens" assertNoFailedTransactions E.scenario3,
            checkPredicateOptions options "Full scenario (lock NFT with minting, mint more tokens, add more NFTs, return all NFTs in exchange of tokens" assertNoFailedTransactions E.scenario4,
            -- validation error scenarios
            checkPredicateOptions options "AuthorizedPubKeys must be a list of unique values" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["authorizedPubKeys has duplicate values", "Script datum incorrectly built", "PT5"] _) -> True; _ -> False)) E.duplicatePubKeys,
            checkPredicateOptions options "Can't return the nft if fractional tokens aren't burned" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Didn't mint/burn exactly fraction tokens and one validity token"] _) -> True; _ -> False)) $ E.returnNFTNoTotalBurn 0,
            checkPredicateOptions options "Can't return the nft if not all fractional tokens aren't burned" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Fraction tokens not burned", "PT5"] _) -> True; _ -> False)) $ E.returnNFTNoTotalBurn 5,
            checkPredicateOptions options "Can't mint fractional tokens" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Datum not updated forging tokens", "PT5"] _) -> True; _ -> False)) E.mintExtraTokens,
            checkPredicateOptions options "Can't add unsigned nft" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Not enough signatures to add tokens", "PT5"] _) -> True; _ -> False)) E.unsignedNFT,
            checkPredicateOptions options "Can't mint unsigned tokens" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Not enough signatures for minting", "PT5"] _) -> True; _ -> False)) E.unsignedMinting,
            checkPredicateOptions options "Can't steal token when not minting or burning" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Token already present", "Token addition incorrect", "PT5"] _) -> True; _ -> False)) E.stealWhenAdding,
            checkPredicateOptions options "Can't steal NFTs when minting" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Contract value not preserved", "PT5"] _) -> True; _ -> False)) E.mintAndSteal,
            checkPredicateOptions options "Can't mint extraneous tokens" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Didn't mint exactly fraction tokens", "PT5"] _) -> True; _ -> False)) E.mintExtraneousTokens,
            checkPredicateOptions options "Can't add the same NFT" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Token already present", "Token addition incorrect", "PT5"] _) -> True; _ -> False)) E.repeatToken,
            checkPredicateOptions options "Can't change datum when adding NFTs" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Datum should not change when adding NFTs", "PT5"] _) -> True; _ -> False)) E.addNFTChangeDatum,
            checkPredicateOptions options "Must return all NFTs" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Script counts incorrect", "PT5"] _) -> True; _ -> False)) E.dontRemoveAll ,
            checkPredicateOptions options "Must record correct fraction tokens asset class" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["datum fractionAC incorrect","Script datum incorrectly built", "PT5"] _) -> True; _ -> False)) E.maliciousMintingPolicy,
            checkPredicateOptions options "Can't add Fraction Token to the bag" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Token is a fraction token", "Token addition incorrect", "PT5"] _) -> True; _ -> False)) E.addFractionToken,
            checkPredicateOptions options "Can't add more NFTs than limit" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["Cannot add this amount of tokens, limit exceeded", "PT5"] _) -> True; _ -> False)) E.transactionLimitExceeded,
            -- minting error scenarios
            checkPredicateOptions options "Can't mint different than declared" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["emittedFractions incorrect","Script datum incorrectly built", "PT5"] _) -> True; _ -> False)) E.initialExtraMint,
            checkPredicateOptions options "Can't mint different than declared when inputting arbitrary UTxO with a datum" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["emittedFractions incorrect","Script datum incorrectly built", "PT5"] _) -> True; _ -> False)) E.initialExtraMintWithArbitraryDatumUtxo,
            checkPredicateOptions options "Can't mint without creating contract UTxO" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["incorrect number of script outputs found"] _) -> True; _ -> False)) E.mintingWithoutContract,
            checkPredicateOptions options "Can't burn if NFTs not returned" (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["incorrect number of script outputs found"] _) -> True; _ -> False)) E.burnNoNFT
        ]
