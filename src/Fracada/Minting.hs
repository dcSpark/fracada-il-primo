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

module Fracada.Minting where

import           Fracada.Constants   as C
import           Fracada.Context
import           Fracada.Validator
import           Fracada.Utils
import           Ledger               hiding (singleton, txInputs, txOutputs, txMint)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import qualified PlutusTx
import           PlutusTx.IsData
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)

data MintingRedeemer
  = InitialMint TxOutRef
    | MintMoreFractions
    | Burn

PlutusTx.makeLift ''MintingRedeemer
PlutusTx.makeIsDataIndexed ''MintingRedeemer [('InitialMint, 0), ('MintMoreFractions, 1), ('Burn, 2)]

{-# INLINEABLE extractMintedAmt #-}
extractMintedAmt :: TokenName -> [(TokenName, Integer)] -> Integer
extractMintedAmt exTokenName mintedTokens =
  snd (fromMaybe (debugError "Didn't mint/burn exactly fraction tokens and one validity token") $ find (\(exTokenName', _) -> exTokenName' == exTokenName) mintedTokens)

-- |
--  Minting policy for minting fraction tokens and validity token
--  Fraction tokens' TokenName is calculated from UTxO spent in the initial transaction
--   and because of that is unique for each Token Bag.
--  Validity token's TokenName is constant, which means it's a fungible token
--   it can only be minted when Token Bag is created by locking initial token
--   and must be burned once that Bag is destroyed by returning all tokens from the Bag
--   and burning all minted fraction tokens.
--  Minting policy is parametrized with Validator,
--   which means that fungible validity tokens are unique for each different validator
{-# INLINEABLE fracadaMintingPolicy #-}
fracadaMintingPolicy :: ValidatorHash -> BuiltinData -> ScriptContext -> Bool
fracadaMintingPolicy fracadaScript rawRedeemer ctx =
  case parsedRedeemer of
    InitialMint utxo -> validateInitialMint fracadaScript utxo ctx
    MintMoreFractions -> validateMintFractionTokens fracadaScript ctx
    Burn -> validateBurn fracadaScript ctx
    where
      parsedRedeemer = fromMaybe (debugError "Redeemer not found") $ fromBuiltinData rawRedeemer

-- |
--  Mints initial fraction tokens and validity token as part of creating the Token Bag
--  UTxO is passed as a redeemer and is used to construct fraction token name
--  Validity token is minted and is used to guarantee initial datum validity
--  Additionally, it is checked that this transaction doesn't consume any input from the script
--    and produces only one script output, with validated datum
{-# INLINEABLE validateInitialMint #-}
validateInitialMint :: ValidatorHash -> TxOutRef -> ScriptContext -> Bool
validateInitialMint fracadaScript utxo ctx =
  let sctx@StandardContext {info, txOutputs, txMint} = extractStandardContext ctx
      ownCS = ownCurrencySymbol ctx
      fractionTokenName = TokenName $ calculateFractionTokenNameHash utxo
      extractedMintedTokens = extractMintedTokens ownCS txMint
      fractionTokensMintedAmount = extractMintedAmt fractionTokenName extractedMintedTokens

      (fracadaValue, fracadaDatumHash) = findSingleScriptOutput fracadaScript txOutputs
      fracadaDatum :: FracadaDatum = findDatum' fracadaDatumHash info

      -- require validity token is locked
      validityTokenAC = assetClass ownCS C.fracadaValidityTokenName
      validityTokenLocked = isUnity fracadaValue validityTokenAC

      checkFracadaDatum =
        debugIfFalse "datum fractionAC incorrect" (fractionAC fracadaDatum == assetClass ownCS fractionTokenName)
          && debugIfFalse "emittedFractions incorrect" (emittedFractions fracadaDatum == fractionTokensMintedAmount)
          && debugIfFalse "authorizedPubKeys has duplicate values" (length (authorizedPubKeys fracadaDatum) == length (nub $ authorizedPubKeys fracadaDatum))

      scriptCount = checkScriptIOCounts fracadaScript 0 1 sctx

      checkUTxOSpent = hasUTxO utxo info

    in debugIfFalse "Minted ammount fractions not positive" (fractionTokensMintedAmount > 0)
        && debugIfFalse "Didn't mint exactly fraction tokens and one validity token" (length extractedMintedTokens == 2)
        && debugIfFalse "Didn't mint validity token" (extractMintedAmt C.fracadaValidityTokenName extractedMintedTokens == 1)
        && debugIfFalse "Validity token not locked" validityTokenLocked
        && debugIfFalse "Fraction validity token is not in the script" (isUnity fracadaValue validityTokenAC)
        && debugIfFalse "Script datum incorrectly built" checkFracadaDatum
        && debugIfFalse "UTxO used for token name isn't spent" checkUTxOSpent
        && debugIfFalse "Fraction token name owerflow" (txOutRefIdx utxo < 256)

        && debugIfFalse "Script counts incorrect" scriptCount

-- |
--  Mints additional fraction tokens
--  Checks that only fraction tokens are minted,
--    declared in datum amount is minted
--    and that it is minted for the UTxO that contains the validity token
{-# INLINEABLE validateMintFractionTokens #-}
validateMintFractionTokens :: ValidatorHash -> ScriptContext -> Bool
validateMintFractionTokens fracadaScript ctx =
  let sctx@StandardContext {info, txInputs, txOutputs, txMint} = extractStandardContext ctx
      (oldFracadaValue, oldFracadaDatumHash) = findSingleScriptOutput fracadaScript txInputs
      (newFracadaValue, newFracadaDatumHash) = findSingleScriptOutput fracadaScript txOutputs
      oldFracadaDatum :: FracadaDatum = findDatum' oldFracadaDatumHash info
      newFracadaDatum :: FracadaDatum = findDatum' newFracadaDatumHash info
      fractionTokenName = snd $ unAssetClass $ fractionAC oldFracadaDatum
      -- find the minted value corresponding to this particular fraction token
      fractionTokensMintedAmount = case extractMintedTokens (ownCurrencySymbol ctx) txMint of
        [(fractionTokenName', amt)] | fractionTokenName' == fractionTokenName -> amt
        _ -> debugError "Didn't mint exactly fraction tokens"

      mintOnlyExpectedTokens = txMint == assetClassValue (fractionAC oldFracadaDatum) fractionTokensMintedAmount

      fractionTokenIncreaseFromDatum = emittedFractions newFracadaDatum - emittedFractions oldFracadaDatum
      mintOnlyExpectedFractionTokens = fractionTokensMintedAmount == fractionTokenIncreaseFromDatum

      -- require validity token is locked
      validityTokenAC = assetClass (ownCurrencySymbol ctx) C.fracadaValidityTokenName
      validityTokenLocked = isUnity oldFracadaValue validityTokenAC

      scriptCount = checkScriptIOCounts fracadaScript 1 1 sctx
      canAddMoreTokens = length (flattenValue $ noAdaValue newFracadaValue) <= C.tokensLimit

    -- subsequent fracada datums are checked by fraction validator
    in debugIfFalse "Minted ammount fractions not positive" (fractionTokensMintedAmount > 0)
        && debugIfFalse "Didn't mint exactly fraction tokens" mintOnlyExpectedTokens
        && debugIfFalse "datum not updated forging tokens" mintOnlyExpectedFractionTokens
        && debugIfFalse "Validity token not locked" validityTokenLocked
        && debugIfFalse "Script counts incorrect" scriptCount
        && debugIfFalse "Cannot add more tokens" canAddMoreTokens

-- |
--  Burns all fraction tokens and validity token as part of destroying the Token Bag
--  Checks that this is done for the UTxO containing the validity token
--    and that no UTxO is left on the script
{-# INLINEABLE validateBurn #-}
validateBurn :: ValidatorHash -> ScriptContext -> Bool
validateBurn fracadaScript ctx =
  let sctx@StandardContext {info, txInputs, txMint} = extractStandardContext ctx
      ownCS = ownCurrencySymbol ctx
      (_, fracadaDatumHash) = findSingleScriptOutput fracadaScript txInputs
      fracadaDatum :: FracadaDatum = findDatum' fracadaDatumHash info
      fractionTokenName = snd $ unAssetClass $ fractionAC fracadaDatum

      extractedMintedTokens = extractMintedTokens ownCS txMint
      fractionTokensMintedAmount = extractMintedAmt fractionTokenName extractedMintedTokens

      scriptCount = checkScriptIOCounts fracadaScript 1 0 sctx

  in debugIfFalse "Burned amount fractions not negative" (fractionTokensMintedAmount < 0)
    && debugIfFalse "Fraction tokens not burned" (negate fractionTokensMintedAmount == emittedFractions fracadaDatum)
    && debugIfFalse "Didn't burn one validity token" (extractMintedAmt C.fracadaValidityTokenName extractedMintedTokens == (-1))
    && debugIfFalse "Didn't burn exactly fraction tokens and one validity token" (length extractedMintedTokens == 2)
    && debugIfFalse "Script counts incorrect" scriptCount

fracadaPolicy :: Scripts.MintingPolicy
fracadaPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . fracadaMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode fracadaValidatorHash

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol fracadaPolicy
