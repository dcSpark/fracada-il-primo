
{-# LANGUAGE OverloadedStrings #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus


import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Short      as SBS
import           Data.Foldable              (toList)

import           Ledger                     (PubKey (..))
import           Ledger.Bytes


import           Fracada.Minting
import           Fracada.Validator

import           Data.String                (IsString (..))
import           Plutus.V1.Ledger.Value


loadVkey :: String -> IO (Either (FileError TextEnvelopeError) (VerificationKey PaymentKey))
loadVkey fileName = readFileTextEnvelope (AsVerificationKey AsPaymentKey) fileName

toPubKey :: VerificationKey PaymentKey -> PubKey
toPubKey vKey = PubKey $ Ledger.Bytes.fromBytes $ serialiseToRawBytes vKey

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 3 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "script-dump <NFT currency symbol> <NFT token name> <fraction token name> <min sig required>  < paths to authorized vkey files"
  else
    do
      stdInText <- getContents
      vKeys <- mapM loadVkey $ lines stdInText
      let
        pks =  map toPubKey $ concatMap toList vKeys
        [nftSymbol, nftTokenName', fractionTokenName', minSigs'] = args
        validatorname = "validator.plutus"
        mintingname = "minting.plutus"
        scriptnum = 42
        nftCurrencySymbol = fromString nftSymbol
        nftTokenName = fromString nftTokenName'
        fractionTokenName = fromString fractionTokenName'
        minSigs = read minSigs'

      if length pks < minSigs then
        do
          putStrLn "Not enough Public keys for required minum"
      else
        do
          let
            nft = AssetClass (nftCurrencySymbol, nftTokenName)

            parameters = FractionNFTParameters {initTokenClass= nft,authorizedPubKeys = pks, minSigRequired= toInteger minSigs}

            fractionToken = Plutus.TokenName fractionTokenName

            appliedValidatorScript =fractionValidatorScript parameters

            validatorAsCbor = serialise appliedValidatorScript
            validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
            validatorScript = PlutusScriptSerialised validatorShortBs
            appliedMintingPolicy = mintFractionTokensPolicy parameters fractionToken

            mintingAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintingPolicy
            mintingAsCbor = serialise mintingAsValidator
            mintingScriptShortBs = SBS.toShort . LB.toStrict $ mintingAsCbor
            mintingScript = PlutusScriptSerialised mintingScriptShortBs

          putStrLn $ "Writing output to: " ++ validatorname
          writePlutusScript scriptnum validatorname validatorScript validatorShortBs

          writeFile "validator-hash.txt" (show $ fractionNftValidatorHash parameters)

          putStrLn $ "Writing output to: " ++ mintingname
          writePlutusScript scriptnum mintingname mintingScript mintingScriptShortBs

          writeFile "currency-id.txt" (show $ curSymbol parameters fractionToken)


writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
