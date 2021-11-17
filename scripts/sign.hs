{-# LANGUAGE OverloadedStrings #-}

import           Ledger
import           Prelude
import           System.Environment

import           Cardano.Api

import qualified Plutus.V1.Ledger.Api       as Plutus




import qualified Cardano.Crypto.Wallet      as Crypto
import qualified Data.ByteArray             as BA
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as BL8



loadSkey :: String -> IO (Either (FileError TextEnvelopeError) (SigningKey PaymentExtendedKey))
loadSkey fileName = readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey) fileName

toPrivKey :: SigningKey PaymentExtendedKey -> Crypto.XPrv
toPrivKey (PaymentExtendedSigningKey key) = key


-- sign :: BA.ByteArrayAccess a => a -> Crypto.XPrv -> Signature

instance BA.ByteArrayAccess BL8.ByteString where
    length =
        BA.length . LB.toStrict
    withByteArray =
        BA.withByteArray . LB.toStrict

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 3 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "sign <file to sign> <signing key file> <output file>"
  else
    do
      let
        [fileToSign, sKeyFile, outputFile] = args
      Right sKey <- loadSkey sKeyFile
      dataToSign <- BL8.readFile fileToSign
      let
        privKey = toPrivKey sKey
        Signature signedData = sign dataToSign privKey
      BL8.writeFile outputFile $ LB.fromStrict $  B16.encode $ Plutus.fromBuiltin signedData
