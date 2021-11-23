
import           Ledger
import           Prelude
import           System.Environment

import           Cardano.Api

import qualified Plutus.V1.Ledger.Api       as Plutus


import qualified Cardano.Crypto.Wallet      as Crypto
import           Data.ByteArray             hiding (length)
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Text.Printf                (printf)



loadSkey :: String -> IO (Either (FileError TextEnvelopeError) (SigningKey PaymentExtendedKey))
loadSkey fileName = readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey) fileName

toPrivKey :: SigningKey PaymentExtendedKey -> Crypto.XPrv
toPrivKey (PaymentExtendedSigningKey key) = key


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
      readData <- BL8.readFile fileToSign
      putStrLn "Enter passphrase"
      strPass <- getLine
      let
        pass =  BL8.toStrict $ BL8.pack strPass
        xpriv = toPrivKey sKey
        Right dataToSign = B16.decode $ LB.toStrict readData
        signedData = Crypto.sign pass xpriv dataToSign
      BL8.writeFile outputFile $ LB.fromStrict $  B16.encode $ Crypto.unXSignature signedData

