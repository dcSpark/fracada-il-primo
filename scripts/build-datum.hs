{-# LANGUAGE OverloadedStrings #-}
import           Cardano.Api
import           Cardano.Api.Shelley
import           Data.Aeson
import           Data.Maybe                 (fromJust)
import           Data.String                (IsString (..))
import           Fracada.Validator
import           Ledger                     (datumHash)
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Plutus.V1.Ledger.Value
import           Prelude
import           System.Environment

import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as BL8


decodeDatum :: BL8.ByteString -> FractionNFTDatum
decodeDatum fileContent = let
                              Just jsonData = Data.Aeson.decode fileContent
                              Right scriptData = scriptDataFromJson ScriptDataJsonDetailedSchema jsonData
                              plutusData = toPlutusData scriptData
                              builtinData = Plutus.dataToBuiltinData plutusData
                          in
                              fromJust $ fromBuiltinData builtinData

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 6 &&  nargs /= 4 && nargs /= 3 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "build-datum new <Fraction currency symbol> <Fraction token name> <number of fractions> <NFT currency symbol> <NFT token name>"
      putStrLn $ "     (creates a datum from scratch)"
      putStrLn $ "build-datum add-nft <current datum file> <NFT currency symbol> <NFT token name>"
      putStrLn $ "     (update the datum adding a new NFT)"
      putStrLn $ "build-datum mint-more <current datum file> <number of fractions>"
      putStrLn $ "     (update the datum minting more fractions)"
      putStr   $ "Provided: "
      putStrLn $ show args
  else
    do
      datum <- case args of
            [ "new", fracSymbol, fracTknName, numberOfFractions', nftSymbol, nftTokenName'] ->
              let
                fracCurrencySymbol = fromString fracSymbol
                fracTokenName = fromString fracTknName
                nftCurrencySymbol = fromString nftSymbol
                nftTokenName = fromString nftTokenName'
                numberOfFractions = (read numberOfFractions' )::Integer

                frac = AssetClass (fracCurrencySymbol, fracTokenName)
                nft = AssetClass (nftCurrencySymbol, nftTokenName)
              in
                pure FractionNFTDatum{ tokensClass= frac, totalFractions = numberOfFractions, newNftClass= nft}
            [ "add-nft", currentDatumFile, nftSymbol, nftTokenName'] -> do
              currentDatumContent <- BL8.readFile currentDatumFile
              let
                currentDatum = decodeDatum currentDatumContent
                nftCurrencySymbol = fromString nftSymbol
                nftTokenName = fromString nftTokenName'
                nft = AssetClass (nftCurrencySymbol, nftTokenName)
              pure currentDatum{newNftClass= nft}
            [ "mint-more", currentDatumFile, numberOfFractions' ] -> do
              currentDatumContent <- BL8.readFile currentDatumFile
              let
                currentDatum@FractionNFTDatum{totalFractions = currentFractions} = decodeDatum currentDatumContent
                newFractions = (read numberOfFractions' )::Integer
              pure currentDatum{ totalFractions = currentFractions + newFractions}
            _ -> error "Unrecognized parameters"

      let
        datumToEncode = Plutus.builtinDataToData $ toBuiltinData datum
        DatumHash dhBs = datumHash $ Datum $ toBuiltinData datum
        encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode)
        datHash = LB.fromStrict $  B16.encode $ fromBuiltin dhBs

      putStrLn $ "encoded datum: " ++ show encoded
      putStrLn $ "datum hash: " ++ show datHash
      BL8.writeFile "datum.json"  encoded
      BL8.writeFile "datum-hash.txt"  datHash

