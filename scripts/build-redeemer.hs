{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Api
import           Cardano.Api.Shelley
import           Data.Aeson
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Fracada.Validator
import           Ledger                     (Signature (..))
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Prelude

readSignature :: String -> IO Signature
readSignature fileName = do
  decodedData <- B16.decode <$> LB.toStrict <$> BL8.readFile fileName
  case decodedData of
    Right data' -> return $ Signature $ Plutus.toBuiltin data'
    Left err    -> error err

encodeRedeemer :: Maybe AddToken -> BL8.ByteString
encodeRedeemer redeemer =
  let redeemerAsData = Plutus.builtinDataToData $ toBuiltinData redeemer
   in Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData redeemerAsData)

main :: IO ()
main = do
  putStrLn $ "Usage:"
  putStrLn $ "build-redeemer  < paths to signed datum hash files for each signature"
  stdInText <- getContents
  sigs' <- mapM readSignature $ lines stdInText
  let redeemer = Just $ AddToken sigs'
      encoded = encodeRedeemer redeemer
  putStrLn $ "encoded redeemer: " ++ show encoded
  BL8.writeFile "redeemer.json" encoded
