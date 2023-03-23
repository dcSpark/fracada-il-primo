{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Api
import           Cardano.Api.Shelley
import           Data.Aeson
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.String                (IsString (..))
import           Fracada.Utils
import           Fracada.Validator
import           Ledger                     (datumHash)
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Plutus.V1.Ledger.Value
import           Prelude
import           System.Environment

decodeDatum :: BL8.ByteString -> FracadaDatum
decodeDatum fileContent =
  let jsonData = fromJust' $ Data.Aeson.decode fileContent
      scriptData = fromRight' $ scriptDataFromJson ScriptDataJsonDetailedSchema jsonData
      plutusData = toPlutusData scriptData
      builtinData = Plutus.dataToBuiltinData plutusData
   in fromJust' $ fromBuiltinData builtinData

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if (head args == "new" && nargs < 7) || (head args == "mint-more" && nargs /= 3)
    then do
      putStrLn "Usage:"
      putStrLn "build-datum new <Fraction currency symbol> <utxo hash> <utxo index> <number of fractions> <minimal signatures required> <authorized pubkeys>"
      putStrLn "     (creates a datum from scratch)"
      putStrLn "build-datum mint-more <current datum file> <number of fractions>"
      putStrLn "     (update the datum minting more fractions)"
      putStr "Provided: "
      print args
    else do
      datum <- case head args of
        "new" -> do
          let fracSymbol         = args !! 1
              txOutHash'         = args !! 2
              txOutIx'           = args !! 3
              numberOfFractions' = args !! 4
              minSigs'           = args !! 5
              pubKeys'           = drop 6 args

              fracCurrencySymbol = fromString fracSymbol
              txOutHash          = fromString txOutHash'
              txOutIx            = read txOutIx' :: Integer
              numberOfFractions  = read numberOfFractions' :: Integer

              minSigs = read minSigs' :: Integer
              pubKeys = map fromString pubKeys' :: [PubKeyHash]

              utxo              = TxOutRef {txOutRefId = txOutHash, txOutRefIdx = txOutIx}
              fractionTokenName = TokenName {unTokenName = calculateFractionTokenNameHash utxo}
              frac = AssetClass (fracCurrencySymbol, fractionTokenName)
           in pure FracadaDatum {fractionAC = frac, emittedFractions = numberOfFractions, authorizedPubKeys = pubKeys, minSigRequired = minSigs}
        "mint-more" -> do
          let currentDatumFile = args !! 1
              numberOfFractions' = args !! 2
          currentDatumContent <- BL8.readFile currentDatumFile
          let currentDatum@FracadaDatum {emittedFractions = currentFractions} = decodeDatum currentDatumContent
              newFractions = read numberOfFractions' :: Integer
          pure currentDatum {emittedFractions = currentFractions + newFractions}
        _ -> error "Unrecognized parameters"

      let datumToEncode = Plutus.builtinDataToData $ toBuiltinData datum
          DatumHash dhBs = datumHash $ Datum $ toBuiltinData datum
          encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode)
          datHash = LB.fromStrict $ B16.encode $ fromBuiltin dhBs

      putStrLn $ "encoded datum: " ++ show encoded
      putStrLn $ "datum hash: " ++ show datHash
      BL8.writeFile "datum.json" encoded
      BL8.writeFile "datum-hash.txt" datHash
