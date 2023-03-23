{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-local-binds #-}

module Trace where

import           Control.Monad          hiding (fmap)
import           Data.Default           (Default (..))
import qualified Data.Map               as Map
import           Fracada.Minting
import           Fracada.Offchain       as OC
import           Fracada.Utils
import           Fracada.Validator
import           Ledger                 hiding (singleton)
import           Ledger.Ada             as Ada
import           Ledger.CardanoWallet   as CW
import qualified Ledger.Crypto          as Crypto
import           Ledger.Value           as Value
import           Plutus.Contract.Test
import           Plutus.Trace
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.IsData
import           PlutusTx.Prelude       hiding (Semigroup (..), unless, trace)
import           Prelude                ((<>), IO)
import           Spec.EvilEndpoints     as Evil
import           Wallet.Emulator.Wallet

nftCurrency :: CurrencySymbol
nftCurrency = "bb7cd5359aa4de1dc9725fb7d8283922185d1cdbfe5fdf35df46c028"

nftName :: TokenName
nftName = "NFT"

nft :: AssetClass
nft = AssetClass (nftCurrency, nftName)

nft2 :: AssetClass
nft2 = assetClass "bb7cd5359aa4de1dc9725fb7d8283922185d1cdbfe5fdf35df46c028" "NFT2"

nft3 :: AssetClass
nft3 = assetClass "bb7cd5359aa4de1dc9725fb7d8283922185d1cdbfe5fdf35df46c028" "NFT3"

wallets :: [Wallet]
wallets = [w1, w2, w3]

privKeys :: [PaymentPrivateKey]
privKeys = map (paymentPrivateKey . fromJust' . walletToMockWallet) wallets

pubKeysHashes :: [PubKeyHash]
pubKeysHashes = map unPaymentPubKeyHash (mockWalletPaymentPubKeyHash <$> wallets)

minSigns :: Integer
minSigns = 2

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(w, v) | w <- [w1, w2, w3]]) def
  where
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue nft 1 <> assetClassValue nft2 2 <> assetClassValue nft3 1

trace :: EmulatorTrace ()
trace = do
  h1 <- activateContractWallet w1 OC.endpoints
  h2 <- activateContractWallet w1 Evil.endpoints
  void $ Emulator.waitNSlots 1
  let toFraction = ToFraction {fractions = 10, pubKeys = pubKeysHashes, minSigs = minSigns, initTokenClass = nft}

  callEndpoint @"fractionNFT" h1 toFraction
  void $ Emulator.waitNSlots 1

  callEndpoint @"returnNFTNoFrac" h2 (0, nft)
  void $ Emulator.waitNSlots 1


main :: IO ()
main = runEmulatorTraceIO' def emCfg trace
