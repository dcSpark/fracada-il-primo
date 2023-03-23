
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus

import qualified Data.ByteString.Lazy       as LB

import qualified Data.ByteString.Short      as SBS

import           Fracada.Minting
import           Fracada.Validator

-- toPrivKey :: SigningKey PaymentExtendedKey -> Crypto.XPrv
-- toPrivKey (PaymentExtendedSigningKey key) = key

-- xPubToPublicKey :: Crypto.XPub -> PubKey
-- xPubToPublicKey = PubKey . KB.fromBytes . Crypto.xpubPublicKey

main :: IO ()
main = do
  let
    validatorname = "validator.plutus"
    mintingname = "minting.plutus"
    scriptnum = 42


    validatorAsCbor = serialise fractionValidatorScript
    validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
    validatorScript = PlutusScriptSerialised validatorShortBs

    mintingAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript fracadaPolicy
    mintingAsCbor = serialise mintingAsValidator
    mintingScriptShortBs = SBS.toShort . LB.toStrict $ mintingAsCbor
    mintingScript = PlutusScriptSerialised mintingScriptShortBs

  putStrLn $ "Writing output to: " ++ validatorname
  writePlutusScript scriptnum validatorname validatorScript validatorShortBs

  writeFile "validator-hash.txt" (show fracadaValidatorHash)

  putStrLn $ "Writing output to: " ++ mintingname
  writePlutusScript scriptnum mintingname mintingScript mintingScriptShortBs

  writeFile "currency-id.txt" (show curSymbol)


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
