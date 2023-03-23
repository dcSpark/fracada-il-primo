{-# LANGUAGE NoImplicitPrelude #-}

module Fracada.Validator where

import           Fracada.Constants      as C
import           Fracada.Context
import           Fracada.Utils
import           GHC.Generics           (Generic)
import           Ledger                 hiding (singleton, txMint, txInputs, txOutputs)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import qualified PlutusTx               (compile, makeIsDataIndexed,
                                         makeLift)
import           PlutusTx.Builtins      as Builtins
import           PlutusTx.Prelude       as P
import           Prelude                (Show)

data FracadaDatum = FracadaDatum
  { fractionAC     :: !AssetClass,
    emittedFractions :: !Integer,
    authorizedPubKeys :: ![PubKeyHash],
    minSigRequired    :: !Integer
  }
  deriving (Generic, Show)

instance Eq FracadaDatum where
  {-# INLINEABLE (==) #-}
  FracadaDatum {fractionAC, emittedFractions, authorizedPubKeys, minSigRequired} ==
    FracadaDatum {fractionAC = fractionAC', emittedFractions = emittedFractions', authorizedPubKeys = authorizedPubKeys', minSigRequired = minSigRequired'} =
      (fractionAC == fractionAC') &&
      (emittedFractions == emittedFractions') &&
      (authorizedPubKeys == authorizedPubKeys') &&
      (minSigRequired == minSigRequired')

PlutusTx.makeLift ''FracadaDatum
PlutusTx.makeIsDataIndexed ''FracadaDatum [('FracadaDatum, 0)]

-- |
--  Fracada validator
--  Validates the transaction based on the amount of fraction tokens that are minted in the transaction
{-# INLINEABLE fracadaValidator #-}
fracadaValidator :: FracadaDatum -> () -> ScriptContext -> Bool
fracadaValidator inputDatum@FracadaDatum {fractionAC} _ ctx
  | forgedFractionTokens > 0 = validateMintingFractions inputDatum sctx valHash
  | forgedFractionTokens < 0 = validateReturningAndBurning inputDatum sctx valHash
  | otherwise                = validateAddingTokens inputDatum sctx valHash
    where
      sctx@StandardContext {txMint} = extractStandardContext ctx
      forgedFractionTokens = assetClassValueOf txMint fractionAC
      valHash = ownHash ctx

-- |
--  Validates minting fractions,
--   with correct update to the emittedFractions in the datum,
--   and the transaction must be signed by minimum required amount of authorized signers.
--  Non-Ada value in the script must be preserved (cannot withdraw tokens from the script)
{-# INLINEABLE validateMintingFractions #-}
validateMintingFractions :: FracadaDatum -> StandardContext -> ValidatorHash -> Bool
validateMintingFractions FracadaDatum {fractionAC, emittedFractions, authorizedPubKeys, minSigRequired} sctx@StandardContext {info, txMint, txInputs, txOutputs} valHash =
  let (oldFracadaValue, _inputDatumHash) = findSingleScriptOutput valHash txInputs
      (newFracadaValue, outputDatumHash) = findSingleScriptOutput valHash txOutputs
      forgedFractionTokens = assetClassValueOf txMint fractionAC
      noAdaValuePreserved = noAdaValue newFracadaValue == noAdaValue oldFracadaValue
      FracadaDatum {fractionAC = fractionAC', emittedFractions = emittedFractions', authorizedPubKeys = authorizedPubKeys', minSigRequired = minSigRequired'} = findDatum' outputDatumHash info

      datumUpdated = fractionAC' == fractionAC &&
        authorizedPubKeys' == authorizedPubKeys &&
        minSigRequired' == minSigRequired &&
        emittedFractions' == (emittedFractions + forgedFractionTokens)

      scriptCount = checkScriptIOCounts valHash 1 1 sctx
  in debugIfFalse "Not enough signatures for minting" (validateSignatures authorizedPubKeys minSigRequired info)
        && debugIfFalse "Contract value not preserved" noAdaValuePreserved
        && debugIfFalse "Datum not updated forging tokens" datumUpdated
        && debugIfFalse "Script counts incorrect" scriptCount

-- |
--  Validates returning the bag's tokens and burning fraction tokens.
--  All emitted fraction tokens and the validity token must be burned,
--   and there is no output script UTxO (all the tokens in the bag must be returned).
{-# INLINEABLE validateReturningAndBurning #-}
validateReturningAndBurning :: FracadaDatum -> StandardContext -> ValidatorHash -> Bool
validateReturningAndBurning FracadaDatum {fractionAC, emittedFractions} sctx@StandardContext {txMint} valHash =
  let forgedFractionTokens = assetClassValueOf txMint fractionAC
      validityTokenAC = getValidityTokenAC fractionAC

      fractionTokensBurnt = (forgedFractionTokens == negate emittedFractions)
      validityTokenBurned = checkSingleTokenIsBurned validityTokenAC txMint
      scriptCount = checkScriptIOCounts valHash 1 0 sctx
  in debugIfFalse "Fraction tokens not burned" fractionTokensBurnt
        && debugIfFalse "Validity token not burned" validityTokenBurned
        && debugIfFalse "Script counts incorrect" scriptCount

-- |
--  Validates adding tokens to the bag,
--   with no update to the datum,
--   and the transaction must be signed by minimum required amount of authorized signers.
--  Tokens that are being added must not already be in the bag (even with negative value, which prevents spending),
--   and must not be fraction tokens
--  As the minting policy is not used in this transaction,
--   this function must also check for the validity token in the script.
--  There is also a protection to prevent the resulting UTxO being too big to spend.
{-# INLINEABLE validateAddingTokens #-}
validateAddingTokens :: FracadaDatum -> StandardContext -> ValidatorHash -> Bool
validateAddingTokens inputDatum@FracadaDatum {fractionAC, authorizedPubKeys, minSigRequired} sctx@StandardContext {info, txInputs, txOutputs} valHash =
  let (oldFracadaValue, _inputDatumHash) = findSingleScriptOutput valHash txInputs
      (newFracadaValue, outputDatumHash) = findSingleScriptOutput valHash txOutputs
      validityTokenAC = getValidityTokenAC fractionAC
      outputDatum = findDatum' outputDatumHash info

      tokenCanBeAdded (currencyId', tokenName', _amount) =
        debugIfFalse "Token already present" (assetClassValueOf oldFracadaValue tokenAsset == 0)
        && debugIfFalse "Token is a fraction token" (tokenAsset /= fractionAC)
        where
          tokenAsset = assetClass currencyId' tokenName'

      noAdaIncrease = flattenValue $ noAdaValue (newFracadaValue - oldFracadaValue)
      scriptCount = checkScriptIOCounts valHash 1 1 sctx
      canAddMoreTokens = length (flattenValue $ noAdaValue newFracadaValue) <= C.tokensLimit
      validityTokenInInput = isUnity oldFracadaValue validityTokenAC
      validityTokenInOutput = isUnity newFracadaValue validityTokenAC
  in debugIfFalse "Not enough signatures to add tokens" (validateSignatures authorizedPubKeys minSigRequired info)
    && debugIfFalse "Token addition incorrect" (all tokenCanBeAdded noAdaIncrease)
    && debugIfFalse "Datum should not change when adding NFTs" (inputDatum == outputDatum)
    && debugIfFalse "Validity token not in input" validityTokenInInput
    && debugIfFalse "Validity token not in output" validityTokenInOutput
    && debugIfFalse "Script counts incorrect" scriptCount
    && debugIfFalse "Cannot add this amount of tokens, limit exceeded" canAddMoreTokens

-- | Datum and redeemer parameter types for fractioning script
data FracadaValidatorType

instance Scripts.ValidatorTypes FracadaValidatorType where
  type RedeemerType FracadaValidatorType = ()
  type DatumType FracadaValidatorType = FracadaDatum

fracadaValidatorInstance :: Scripts.TypedValidator FracadaValidatorType
fracadaValidatorInstance =
  Scripts.mkTypedValidator @FracadaValidatorType
    $$(PlutusTx.compile [||fracadaValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @FracadaDatum @()

fracadaValidatorHash :: ValidatorHash
fracadaValidatorHash = Scripts.validatorHash fracadaValidatorInstance

fractionValidatorScript :: Validator
fractionValidatorScript = Scripts.validatorScript fracadaValidatorInstance

fracadaValidatorAddress :: Address
fracadaValidatorAddress = Ledger.scriptAddress fractionValidatorScript
