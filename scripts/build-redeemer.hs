{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Api
import           Cardano.Api.Shelley
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Prelude
import           Fracada.Minting
import           System.Environment
import           Data.String

encodeRedeemer :: MintingRedeemer -> BL8.ByteString
encodeRedeemer redeemer =
  let redeemerAsData = Plutus.builtinDataToData $ toBuiltinData redeemer
   in Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData redeemerAsData)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args

  if (nargs == 0 || (head args /= "InitialMint" && head args /= "MintMoreFractions" && head args /= "Burn" )) ||
     (head args == "InitialMint" && nargs /= 3) ||
     (head args == "MintMoreFractions" && nargs /= 1) ||
     (head args == "Burn" && nargs /= 1)
    then do
      putStrLn "Usage:"
      putStrLn "build-redeemer InitialMint <utxo hash> <utxo index>"
      putStrLn "build-redeemer MintMoreFractions"
      putStrLn "build-redeemer Burn"
      putStr "Provided: "
      print args

    else do
      redeemer <- case head args of
        "InitialMint" -> do
          let txOutHash' = args !! 1
              txOutHash  = fromString txOutHash'
              txOutIx'   = args !! 2
              txOutIx    = read txOutIx' :: Integer
            in pure $ InitialMint TxOutRef {txOutRefId = txOutHash, txOutRefIdx = txOutIx}
        "MintMoreFractions" -> pure MintMoreFractions
        "Burn" -> pure Burn
        _ -> error "Unrecognized parameters"

      let encoded = encodeRedeemer redeemer
      putStrLn $ "encoded redeemer: " ++ show encoded
      BL8.writeFile "redeemer.json" encoded
