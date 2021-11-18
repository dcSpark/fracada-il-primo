{-# LANGUAGE OverloadedStrings #-}
import           Cardano.Api
import           Cardano.Api.Shelley
import           Data.Aeson
import           Data.String                (IsString (..))
import           Fracada.Validator
import           Ledger                     (Signature (..))
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Plutus.V1.Ledger.Value
import           Prelude
import           System.Environment

import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as BL8


readSignature :: String -> IO Signature
readSignature fileName =  do
                            decodedData <- B16.decode <$> LB.toStrict <$> BL8.readFile fileName
                            case decodedData of
                              Right data' -> return $ Signature $ Plutus.toBuiltin data'
                              Left err -> error err

encodeRedeemer :: Maybe AddToken -> BL8.ByteString
encodeRedeemer redeemer = let
                    redeemerAsData = Plutus.builtinDataToData $ toBuiltinData redeemer
                  in
                    Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData redeemerAsData)
main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 2 && nargs /= 0 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "build-redeemer <currency symbol> <token name> < paths to signed datum hash files for each signature"
      putStrLn $ "OR"
      putStrLn $ "build-redeemer"
      putStr   $ "Provided: "
      putStrLn $ show args
  else if nargs == 0 then
    do
      putStrLn $ "Building empty redeemer"
      BL8.writeFile "redeemer.txt"  $ encodeRedeemer Nothing
  else
    do
      stdInText <- getContents
      sigs' <- mapM readSignature $ lines stdInText
      let
        [ tokenSymbol', tokenName'] = args
        newToken' = AssetClass ( fromString tokenSymbol', fromString tokenName')
        redeemer = Just AddToken{newToken = newToken', signatures' = sigs'}
        encoded = encodeRedeemer redeemer
      putStrLn $ "encoded redeemer: " ++ show encoded
      BL8.writeFile "redeemer.txt"  encoded

