{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE CPP #-}

module Fracada.Utils where

import           Fracada.Constants      as C
import           Fracada.Context
import           Ledger            hiding (datumHash, singleton, value, txInputs, txOutputs)
import           Ledger.Value
import           PlutusTx.Builtins as Builtins
import           PlutusTx.Prelude  as P
import qualified PlutusTx
import           Plutus.V1.Ledger.Credential (Credential (..))

fromJust' :: Maybe a -> a
fromJust' (Just a) = a
fromJust' _        = error ()

fromRight' :: Either b a -> a
fromRight' (Right a) = a
fromRight' _         = error ()

{-# INLINEABLE checkSingleTokenIsBurned #-}
checkSingleTokenIsBurned :: AssetClass -> Value -> Bool
checkSingleTokenIsBurned ac txMint = assetClassValueOf txMint ac == -1

{-# INLINEABLE extractMintedTokens #-}
extractMintedTokens :: CurrencySymbol -> Value -> [(TokenName, Integer)]
extractMintedTokens mintedSymbol txMint =
  [(tn, amt) | (cs, tn, amt) <- flattenValue txMint, cs == mintedSymbol]

{-# INLINEABLE isUnity #-}
isUnity :: Value -> AssetClass -> Bool
isUnity value asset = assetClassValueOf value asset == 1

{-# INLINEABLE findSingleScriptOutput #-}
findSingleScriptOutput :: ValidatorHash -> [TxOut] -> (Value, DatumHash)
findSingleScriptOutput wantedScriptValidatorHash txOuts =
  if singleOutputPresent
    then head scriptOutputs
    else debugError "incorrect number of script outputs found"
  where
    scriptOutputs = findScriptOutputs wantedScriptValidatorHash txOuts
    singleOutputPresent = length scriptOutputs == 1

{-# INLINEABLE checkScriptIOCounts #-}
checkScriptIOCounts :: ValidatorHash -> Integer -> Integer -> StandardContext -> Bool
checkScriptIOCounts vh expectedInputCount expectedOutputCount StandardContext {txInputs, txOutputs} =
  length (findScriptOutputs vh txInputs) == expectedInputCount
  && length (findScriptOutputs vh txOutputs) == expectedOutputCount

{-# INLINEABLE findScriptOutputs #-}
findScriptOutputs :: ValidatorHash -> [TxOut] -> [(Value, DatumHash)]
findScriptOutputs wantedScriptValidatorHash txOuts =
  [ (txOutValue, fromMaybe (debugError "expected datum hash") txOutDatumHash)
    | TxOut {txOutValue, txOutDatumHash, txOutAddress = Address {addressCredential}} <- txOuts,
      addressCredential == ScriptCredential wantedScriptValidatorHash
  ]

{-# INLINEABLE findDatum' #-}
findDatum' :: PlutusTx.FromData a => DatumHash -> TxInfo -> a
findDatum' datumHash info =
  fromMaybe (debugError "datum not found") $
    PlutusTx.fromBuiltinData . getDatum . fromMaybe (debugError "datum not found") $
    findDatum datumHash info

{-# INLINABLE calculateFractionTokenNameHash #-}
calculateFractionTokenNameHash :: TxOutRef -> BuiltinByteString
calculateFractionTokenNameHash utxo =
  sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> TxInfo -> Bool
hasUTxO utxo info = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

{-# INLINEABLE validateSignatures #-}
validateSignatures :: [PubKeyHash] -> Integer -> TxInfo -> Bool
validateSignatures pubKeys minSignatures info =
  let uniqueTxSignatories = nub (txInfoSignatories info)
      isAuthorized pkh = pkh `elem` pubKeys
      matches = length $ filter isAuthorized uniqueTxSignatories
   in matches >= minSignatures

{-# INLINEABLE getValidityTokenAC #-}
getValidityTokenAC :: AssetClass -> AssetClass
getValidityTokenAC fractionAC = assetClass validityCS C.fracadaValidityTokenName
  where AssetClass (validityCS, _) = fractionAC

{-# INLINABLE debugIfFalse #-}
debugIfFalse :: BuiltinString -> Bool -> Bool
#if defined(DEBUG)
-- Use this definition in Development/Testing
debugIfFalse = traceIfFalse
#else
-- Use this definition in Production
debugIfFalse _debugMessage val = val
#endif

{-# INLINABLE debugError #-}
debugError :: BuiltinString -> a
#if defined(DEBUG)
-- Use this definition in Development/Testing
debugError = traceError
#else
-- Use this definition in Production
debugError _debugMessage = error ()
#endif