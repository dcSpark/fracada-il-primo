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

import qualified Data.ByteString.Char8   as C
import qualified Data.Text               as Text
import           GHC.Generics            (Generic)
import           Ledger                  hiding (singleton)
import           Ledger.Ada              (fromValue, toValue)
import qualified Ledger.Crypto           as Crypto
import qualified Ledger.Typed.Scripts    as Scripts
import           Ledger.Value            as Value
import           Plutus.V1.Ledger.Bytes
import qualified PlutusTx
import           PlutusTx.Builtins       as Builtins
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import           PlutusTx.Prelude        as P
import           PlutusTx.Prelude        hiding (Semigroup (..), unless)
import           Prelude                 (Show, String, show)

import qualified Data.ByteString         as B
import qualified Data.ByteString.Base16  as B16
import qualified Data.Text.Encoding      as TE
-- import Data.ByteString hiding (length, filter)
--import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Lazy    as LB


newtype MessageHash =
    MessageHash Builtins.BuiltinByteString
    deriving newtype (Show)
PlutusTx.makeLift ''MessageHash

data AddToken = AddToken {
      newToken    :: AssetClass,
      signatures' :: ![Signature]
    } deriving (Generic, Show)

PlutusTx.makeLift ''AddToken
PlutusTx.makeIsDataIndexed ''AddToken [('AddToken,0)]

{-# INLINABLE hashMessage #-}
hashMessage :: Message -> MessageHash
hashMessage (Message bsMsg) = MessageHash $ sha2_256 bsMsg

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
      nftCount       :: !Integer
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
  :: MessageHash
  -- ^ The hash of the message
  -> PubKey
  -- ^ The public key of the signatory
  -> Signature
  -- ^ The signed message
  -> Bool
checkSignature (MessageHash bsHash) (PubKey (LedgerBytes pk))  (Signature sig) =
        verifySignature pk bsHash sig

-- | Sign a message hash with a private key
{-# INLINABLE sign #-}
sign :: MessageHash -> PrivateKey -> Signature
sign (MessageHash bsHash) pk = Crypto.sign bsHash pk

{-# INLINABLE validateSignatures #-}
validateSignatures :: [PubKey] -> Integer -> [Signature] -> DatumHash -> Bool
validateSignatures pubKeys minSignatures sigs dh = let
                        msgHash = hashMessage msg
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

toTxt :: String -> BuiltinByteString
toTxt =  toBuiltin . TE.encodeUtf8 . Text.pack

{-# INLINABLE fractionNftValidator #-}
fractionNftValidator :: FractionNFTParameters -> FractionNFTDatum -> Maybe AddToken -> ScriptContext -> Bool
fractionNftValidator FractionNFTParameters{initTokenClass = nftAsset, authorizedPubKeys, minSigRequired } FractionNFTDatum{tokensClass, totalFractions, nftCount} redeemer ctx =
  let
    txInfo = scriptContextTxInfo ctx
    valueInContract = valueLockedBy txInfo (ownHash ctx)
    currentValueInContract = valueWithin $ findOwnInput' ctx
    forgedTokens = assetClassValueOf (txInfoMint txInfo) tokensClass
    outputDatum :: FractionNFTDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h txInfo of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"
  in
    if isJust redeemer then
      -- adding nfts or minting, signatures are required
      let
        Just AddToken {newToken, signatures' } = redeemer
        (newTokenCurrId, newTokenTokenNm) = unAssetClass newToken

        FractionNFTDatum{tokensClass=tc', totalFractions= tf', nftCount=nftc' } = outputDatum
        [(currencyId, tokenName, value)] = flattenValue $ valueInContract - (toValue $ fromValue valueInContract) - currentValueInContract
        valueIncreaseMatchToken = currencyId == newTokenCurrId && newTokenTokenNm == tokenName
        -- validate the datum hash is properly signed
        requiredSignatures = validateSignatures authorizedPubKeys minSigRequired signatures'
      in
        if forgedTokens >0 then
            -- minting more tokens
            let
              -- keep the NFTs
              valuePreserved = valueInContract == currentValueInContract
              -- update total count
              datumUpdated = tc' == tokensClass && tf' == totalFractions + forgedTokens && nftCount == nftc'

              expectedMinted = assetClassValue tokensClass forgedTokens

            in
              traceIfFalse "message doesn't match" (  toTxt strMsg == toTxt  strMsg ) &&
              traceIfFalse "not enough signatures for minting"  requiredSignatures &&
              traceIfFalse "contract value not preserved"  valuePreserved  &&
              traceIfFalse "datum not updated"  datumUpdated &&
              traceIfFalse "Unexpected minted amount" (txInfoMint txInfo == expectedMinted) &&  -- I should send the total amount expected
              traceIfFalse "Unexpected token" valueIncreaseMatchToken
        else
          -- add new tokens to the lock
          let
            newTokenValueOf :: Value -> Integer
            newTokenValueOf val = assetClassValueOf val newToken

            oldValue = newTokenValueOf $ valueWithin $ findOwnInput' ctx
            newValue = newTokenValueOf valueInContract
            valueOfMessageAsset = assetClassValueOf
            valueIncreased = oldValue  < newValue

            unMessage (Message bs) = bs


          in
            traceIfFalse "not enough signatures for redeeming"  requiredSignatures
            && traceIfFalse "no new value " (not $ isZero $ valueProduced txInfo )
            && traceIfFalse "token preserved " (not $ isZero $ valueInContract )
            && traceIfFalse "Tokens not added" valueIncreased
            && traceIfFalse "datum not preserved" ( tokensClass == tc' && totalFractions == tf' )
            && traceIfFalse "Unexpected token" valueIncreaseMatchToken
    else
      let
          -- make sure the asset(s) are returned
          assetIsReturned = assetClassValueOf (valueProduced txInfo) nftAsset > 0
          tokensBurnt = (forgedTokens == negate totalFractions)  && forgedTokens /= 0
        in
          traceIfFalse "NFT not returned" assetIsReturned &&
          traceIfFalse "Value locked in contract" ( isZero valueInContract ) &&
          traceIfFalse "Tokens not burn" tokensBurnt


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

