{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Fracada.Validator where

import qualified Data.Text              as Text
import           GHC.Generics           (Generic)
import           Ledger                 hiding (singleton)
import           Ledger.Ada             (fromValue, toValue)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.V1.Ledger.Bytes
import qualified PlutusTx
import           PlutusTx.Builtins      as Builtins
import           PlutusTx.Prelude       as P
--import           PlutusTx.Prelude        hiding (Semigroup (..), unless)
import           Prelude                (Show, String)

import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding     as TE

data AddToken = AddToken {
      newToken    :: AssetClass,
      signatures' :: ![Signature]
    } deriving (Generic, Show)

PlutusTx.makeLift ''AddToken
PlutusTx.makeIsDataIndexed ''AddToken [('AddToken,0)]

data FractionNFTParameters = FractionNFTParameters {
      initTokenClass    :: !AssetClass,
      authorizedPubKeys :: ![PubKey],
      minSigRequired    :: !Integer
    } deriving (Generic, Show)

PlutusTx.makeLift ''FractionNFTParameters
PlutusTx.makeIsDataIndexed ''FractionNFTParameters [('FractionNFTParameters,0)]

data FractionNFTDatum = FractionNFTDatum {
      tokensClass    :: !AssetClass,
      totalFractions :: !Integer,
      newNftClass    :: !AssetClass
    } deriving (Generic, Show)

PlutusTx.makeLift ''FractionNFTDatum
PlutusTx.makeIsDataIndexed ''FractionNFTDatum [('FractionNFTDatum,0)]

-- | Datum and redeemer parameter types for fractioning script
data Fractioning
instance Scripts.ValidatorTypes Fractioning where
    type instance RedeemerType Fractioning = Maybe AddToken
    type instance DatumType Fractioning = FractionNFTDatum

-- | Verify the signature on a signed hash
{-# INLINABLE checkSignature #-}
checkSignature
  :: DatumHash
  -- ^ The hash of the message
  -> PubKey
  -- ^ The public key of the signatory
  -> Signature
  -- ^ The signed message
  -> Bool
checkSignature (DatumHash bsHash) (PubKey (LedgerBytes pk))  (Signature sig) =
        verifySignature pk bsHash sig

{-# INLINABLE validateSignatures #-}
validateSignatures :: [PubKey] -> Integer -> [Signature] -> DatumHash -> Bool
validateSignatures pubKeys minSignatures sigs msgHash = let
                        uniquePubKeys = nub pubKeys
                        uniqueSigs = nub sigs
                        allComb =  [(pk, sig) | pk <- uniquePubKeys, sig <- uniqueSigs ]
                        isValidSig (pk,sig) = checkSignature msgHash pk sig
                        matches = length $ filter isValidSig allComb
                      in
                        matches >= minSignatures


{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE datumToData #-}
datumToData :: (PlutusTx.FromData a) => Datum -> Maybe a
datumToData datum = PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData (getDatum datum)

{-# INLINABLE findNewOwnDatum #-}
findNewOwnDatum :: ScriptContext -> Datum
findNewOwnDatum ctx = let
              txInfo = scriptContextTxInfo ctx
              ownDatumHash = snd $ ownHashes ctx
              oDatum = findDatum ownDatumHash txInfo
            in
              case oDatum of
                Just datum -> datum
                Nothing    -> traceError "New datum not found"



fromCurrencySymbol :: CurrencySymbol -> String
fromCurrencySymbol =  Text.unpack . TE.decodeUtf8 . B16.encode . fromBuiltin . unCurrencySymbol

fromTokenName :: TokenName -> String
fromTokenName = fromBBS . unTokenName

fromBBS :: BuiltinByteString -> String
fromBBS = Text.unpack . TE.decodeUtf8 . fromBuiltin


{-# INLINABLE toTxt #-}
toTxt :: String -> BuiltinByteString
toTxt =  toBuiltin . TE.encodeUtf8 . Text.pack

{-# INLINABLE ownNextDatumHash #-}
ownNextDatumHash :: ScriptContext -> DatumHash
ownNextDatumHash ctx =  case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing -> traceError "wrong output type"
            Just h  -> h
        _   -> traceError "expected exactly one continuing output"

{-# INLINABLE fractionNftValidator #-}
fractionNftValidator :: FractionNFTParameters -> FractionNFTDatum -> Maybe AddToken -> ScriptContext -> Bool
fractionNftValidator FractionNFTParameters{initTokenClass = nftAsset, authorizedPubKeys, minSigRequired } FractionNFTDatum{tokensClass, totalFractions, newNftClass} redeemer ctx =
  let
    txInfo = scriptContextTxInfo ctx
    valueInContract = valueLockedBy txInfo (ownHash ctx)
    currentValueInContract = valueWithin $ findOwnInput' ctx
    forgedTokens = assetClassValueOf (txInfoMint txInfo) tokensClass

  in
    if isJust redeemer then
      -- adding nfts or minting, signatures are required
      let
        -- newToken is in the datum too, we should get rid of it
        Just AddToken {newToken, signatures' } = redeemer
        (newTokenCurrId, newTokenTokenNm) = unAssetClass newToken

        ownDatumHash = ownNextDatumHash ctx
        outputDatum = case findDatum ownDatumHash txInfo of
                    Nothing        -> traceError "datum not found"
                    Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just ad' -> ad'
                        Nothing  -> traceError "error decoding data"

        FractionNFTDatum{tokensClass=tc', totalFractions= tf', newNftClass=nftc' } = outputDatum

        -- validate the datum hash is properly signed
        requiredSignatures = validateSignatures authorizedPubKeys minSigRequired signatures' ownDatumHash
      in
        if forgedTokens > 0 then
            -- minting more fractional tokens path
            let
              -- keep the NFTs ( the value locked doesn't change)
              valuePreserved = valueInContract == currentValueInContract
              -- update total count is the only change in the datum
              datumUpdated = tc' == tokensClass && tf' == (totalFractions + forgedTokens) && nftc' == newNftClass
              -- no other token is minted
              mintOnlyExpectedTokens = txInfoMint txInfo == assetClassValue tokensClass forgedTokens
            in
              traceIfFalse "not enough signatures for minting"  requiredSignatures &&
              traceIfFalse "contract value not preserved"  valuePreserved  &&
              traceIfFalse "datum not updated forging tokens"  datumUpdated &&
              traceIfFalse "Unexpected minted amount" mintOnlyExpectedTokens
        else if forgedTokens < 0 then
          -- we don't allow partial burn of tokens
          False
        else
          -- add new tokens to the lock
          let
            newTokenValueOf :: Value -> Integer
            newTokenValueOf val = assetClassValueOf val newToken

            oldValue = newTokenValueOf $ valueWithin $ findOwnInput' ctx
            newValue = newTokenValueOf valueInContract
            --ensure we add only one token each time
            valueIncreased = oldValue +1 == newValue
            -- the latest added token is the only change in the datum
            datumUpdated = tc' == tokensClass && tf' == totalFractions && nftc' == newToken

            --extract the value increase
            adaValue = toValue $ fromValue valueInContract
            [(currencyId', tokenName', increaseAmount)] = flattenValue $ valueInContract - adaValue - currentValueInContract
            -- the value increase should match the new token and should be only 1
            valueIncreaseMatchToken = newTokenCurrId == currencyId' && newTokenTokenNm == tokenName' && increaseAmount == 1

            -- validate the new token is not already added  (TODO: what if some UTxOs aren't included?)
            tokenNotPresent = assetClassValueOf currentValueInContract newNftClass == 0

          in
            traceIfFalse "not enough signatures to add tokens"  requiredSignatures
            && traceIfFalse "no new value " (not $ isZero $ valueProduced txInfo )
            && traceIfFalse "Token already added" tokenNotPresent
            && traceIfFalse "token preserved " (not $ isZero $ valueInContract )
            && traceIfFalse "Tokens not added" valueIncreased
            && traceIfFalse "datum not updated adding NFTs" datumUpdated
            && traceIfFalse "Unexpected token" valueIncreaseMatchToken
    else
      -- return all the NFTs
      let
          -- make sure the asset(s) are returned (TODO: what if some UTxOs aren't included?)
          assetIsReturned = assetClassValueOf (valueProduced txInfo) nftAsset > 0
          -- make sure all the tokens are burned
          tokensBurnt = (forgedTokens == negate totalFractions)  && forgedTokens /= 0
        in
          traceIfFalse "NFT not returned" assetIsReturned &&
          traceIfFalse "Value locked in contract" ( isZero valueInContract ) &&
          traceIfFalse "Tokens not burned" tokensBurnt


fractionNftValidatorInstance ::  FractionNFTParameters -> Scripts.TypedValidator Fractioning
fractionNftValidatorInstance params = Scripts.mkTypedValidator @Fractioning
    ($$(PlutusTx.compile [||  fractionNftValidator ||])
    `PlutusTx.applyCode`  PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @FractionNFTDatum @(Maybe AddToken)

fractionNftValidatorHash :: FractionNFTParameters -> ValidatorHash
fractionNftValidatorHash = Scripts.validatorHash . fractionNftValidatorInstance

fractionValidatorScript :: FractionNFTParameters -> Validator
fractionValidatorScript = Scripts.validatorScript . fractionNftValidatorInstance

fractionNftValidatorAddress :: FractionNFTParameters -> Address
fractionNftValidatorAddress = Ledger.scriptAddress . fractionValidatorScript

