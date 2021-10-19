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

import           Fracada.Validator
import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)


{-# INLINABLE mintFractionTokens #-}
mintFractionTokens :: ValidatorHash -> AssetClass -> Integer -> TokenName -> BuiltinData -> ScriptContext -> Bool
mintFractionTokens fractionNFTScript asset numberOfFractions fractionTokenName _ ctx =
  let
    info = scriptContextTxInfo ctx
    mintedAmount = case flattenValue (txInfoMint info) of
        [(cs, fractionTokenName', amt)] | cs == ownCurrencySymbol ctx && fractionTokenName' == fractionTokenName -> amt
        _                                                           -> 0
  in
    if mintedAmount > 0 then
      let
        lockedByNFTfractionScript = valueLockedBy info fractionNFTScript
        assetIsLocked = assetClassValueOf lockedByNFTfractionScript asset == 1
      in
        traceIfFalse "Asset not locked" assetIsLocked           &&
        traceIfFalse "wrong fraction tokens minted" ( mintedAmount == numberOfFractions)
    else if mintedAmount < 0 then
      let
        -- make sure the asset is spent
        assetIsReturned = assetClassValueOf (valueProduced info) asset > 0
      in
        traceIfFalse "Asset not returned" assetIsReturned           &&
        traceIfFalse "wrong fraction tokens burned" ( mintedAmount == negate numberOfFractions)
    else
      False



mintFractionTokensPolicy :: FractionNFTParameters -> Integer -> TokenName -> Scripts.MintingPolicy
mintFractionTokensPolicy params@FractionNFTParameters{initTokenClass} numberOfFractions fractionTokenName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \validator' asset' numberOfFractions' fractionTokenName' -> Scripts.wrapMintingPolicy $ mintFractionTokens validator' asset' numberOfFractions' fractionTokenName' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode ( fractionNftValidatorHash params)
    `PlutusTx.applyCode`
    PlutusTx.liftCode initTokenClass
    `PlutusTx.applyCode`
    PlutusTx.liftCode numberOfFractions
    `PlutusTx.applyCode`
    PlutusTx.liftCode fractionTokenName

curSymbol ::  FractionNFTParameters -> Integer -> TokenName -> CurrencySymbol
curSymbol params numberOfFractions fractionTokenName = scriptCurrencySymbol $ mintFractionTokensPolicy params numberOfFractions fractionTokenName

