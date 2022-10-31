{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Plutus.V1.Ledger.Api
import           Prelude
import           Fracada.Utils
import           System.Environment
import           Data.String

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args

  if nargs /= 2
    then do
      putStrLn "Usage:"
      putStrLn "build-fraction-tokenname <utxo hash> <utxo index>"
      putStr "Provided: "
      print args
    else do
      let txOutHash' = head args
          txOutHash  = fromString txOutHash'
          txOutIx'   = args !! 1
          txOutIx    = read txOutIx' :: Integer
          encoded = Data.Aeson.encode $ calculateFractionTokenNameHash $ TxOutRef {txOutRefId = txOutHash, txOutRefIdx = txOutIx}
      putStrLn $ "encoded fraction token name: " ++ show encoded
      BL8.writeFile "fraction-tokenname.txt" encoded
