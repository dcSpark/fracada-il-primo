{-# LANGUAGE OverloadedStrings #-}
import           Cardano.Api
import           Cardano.Api.Shelley
import           Data.Aeson
import           Data.String            (IsString (..))
import           Fracada.Validator
import           Ledger                 (datumHash)
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api   as Plutus
import           Plutus.V1.Ledger.Value
import           Prelude
import           System.Environment

-- cabal run datum-dump -- 6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7 FRAC 20 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa nft2

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 5 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "datum-dump <Fraction currency symbol> <Fraction token name> <number of fractions> <NFT currency symbol> <NFT token name>"
  else
    do
      let
        [fracSymbol, fracTknName, numberOfFractions', nftSymbol, nftTokenName'] = args
        fracCurrencySymbol = fromString fracSymbol
        fracTokenName = fromString fracTknName
        nftCurrencySymbol = fromString nftSymbol
        nftTokenName = fromString nftTokenName'
        numberOfFractions = (read numberOfFractions' )::Integer

        frac = AssetClass (fracCurrencySymbol, fracTokenName)
        nft = AssetClass (nftCurrencySymbol, nftTokenName)

        datum =FractionNFTDatum{ tokensClass= frac, totalFractions = numberOfFractions, newNftClass= nft}
        dHash = datumHash $ Datum $ toBuiltinData datum
        datumToEncode = Plutus.builtinDataToData $ toBuiltinData datum
        encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode)

      putStrLn $ "encoded datum: " ++ show encoded
      putStrLn $ "datum hash: " ++ show dHash

