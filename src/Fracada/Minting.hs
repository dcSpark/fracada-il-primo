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
mintFractionTokens :: ValidatorHash -> AssetClass -> TokenName -> Integer -> ScriptContext -> Bool
mintFractionTokens fractionNFTScript asset fractionTokenName numberOfFractions ctx =
  let
    info = scriptContextTxInfo ctx
    -- find the minted value corresponding to this particular fraction token
    mintedAmount = case flattenValue (txInfoMint info) of
        [(cs, fractionTokenName', amt)] | cs == ownCurrencySymbol ctx && fractionTokenName' == fractionTokenName -> amt
        _                                                           -> 0
  in
    if mintedAmount > 0 then
      -- minting
      let
        lockedByNFTfractionScript = valueLockedBy info fractionNFTScript
        -- require that the nft is locked
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
        traceIfFalse "wrong fraction tokens burned" ( mintedAmount == numberOfFractions)
    else
      False



mintFractionTokensPolicy :: FractionNFTParameters -> TokenName -> Scripts.MintingPolicy
mintFractionTokensPolicy params@FractionNFTParameters{initTokenClass} fractionTokenName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \validator' asset' fractionTokenName' -> Scripts.wrapMintingPolicy $ mintFractionTokens validator' asset' fractionTokenName' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode ( fractionNftValidatorHash params)
    `PlutusTx.applyCode`
    PlutusTx.liftCode initTokenClass
    `PlutusTx.applyCode`
    PlutusTx.liftCode fractionTokenName

curSymbol ::  FractionNFTParameters -> TokenName -> CurrencySymbol
curSymbol params fractionTokenName = scriptCurrencySymbol $ mintFractionTokensPolicy params fractionTokenName

