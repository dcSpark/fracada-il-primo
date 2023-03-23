{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Api
import           Cardano.Api.Shelley
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Prelude

encodeRedeemer :: () -> BL8.ByteString
encodeRedeemer redeemer =
  let redeemerAsData = Plutus.builtinDataToData $ toBuiltinData redeemer
   in Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData redeemerAsData)

main :: IO ()
main = do
  putStrLn "Usage:"
  putStrLn "build-redeemer"
  let redeemer = ()
      encoded = encodeRedeemer redeemer
  putStrLn $ "encoded redeemer: " ++ show encoded
  BL8.writeFile "redeemer.json" encoded
