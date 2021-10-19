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

import           GHC.Generics           (Generic)
import           Ledger                 hiding (singleton)
import qualified Ledger.Crypto          as Crypto
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.V1.Ledger.Bytes
import qualified PlutusTx
import           PlutusTx.Builtins      as Builtins
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import           Prelude                (Show)

newtype Message =
    Message Builtins.BuiltinByteString
    deriving newtype (Show)
PlutusTx.makeLift ''Message
PlutusTx.makeIsDataIndexed ''Message [('Message,0)]

newtype MessageHash =
    MessageHash Builtins.BuiltinByteString
    deriving newtype (Show)
PlutusTx.makeLift ''MessageHash

-- this or  ([sig],msg) ?
data AddToken = AddToken {
      newToken    :: AssetClass,
      signatures' :: ![Signature],
      message     :: !Message
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
      totalFractions :: !Integer
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
validateSignatures :: [PubKey] -> Integer -> [Signature] -> Message -> Bool
validateSignatures pubKeys minSignatures sigs msg = let
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

{-# INLINABLE fractionNftValidator #-}
fractionNftValidator :: FractionNFTParameters -> FractionNFTDatum -> Maybe AddToken -> ScriptContext -> Bool
fractionNftValidator FractionNFTParameters{initTokenClass = nftAsset, authorizedPubKeys, minSigRequired } FractionNFTDatum{tokensClass, totalFractions} redeemer ctx =
  let
    txInfo = scriptContextTxInfo ctx
    valueInContract = valueLockedBy txInfo (ownHash ctx)
  in
    if isJust redeemer then
      -- add new tokens to the lock
      let
        Just AddToken {newToken, signatures', message} = redeemer
        newTokenValueOf val = assetClassValueOf val newToken
        oldValue = newTokenValueOf $ valueWithin $ findOwnInput' ctx
        newValue = newTokenValueOf valueInContract
        requiredSignatures = validateSignatures authorizedPubKeys minSigRequired signatures' message
        valueIncreased = oldValue < newValue
        -- base token preserved
      in
        traceIfFalse "not enough signatures"  requiredSignatures &&
        traceIfFalse "no new value " (not $ isZero $ valueProduced txInfo ) &&
        traceIfFalse "token preserved " (not $ isZero $ valueInContract ) &&
        traceIfFalse "Tokens not added" valueIncreased -- do we allow to withdraw tokens (except the original) ?
    else
    let
        -- make sure the asset is spent
        assetIsReturned = assetClassValueOf (valueProduced txInfo) nftAsset > 0
        forgedTokens = assetClassValueOf (txInfoMint txInfo) tokensClass
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

