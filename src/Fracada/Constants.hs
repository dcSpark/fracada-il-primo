{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}

module Fracada.Constants where

import qualified Ledger.Ada   as Ada
import           Ledger.Value
import           Prelude                (Integer)

-- We cannot use Ledger.minAdaTxOut because it's not enough for such a large UTXO
minAda :: Value
minAda = Ada.toValue 3_500_000

{-# INLINEABLE fracadaValidityTokenName #-}
fracadaValidityTokenName :: TokenName
fracadaValidityTokenName = TokenName "FRACADA_VALIDITY"

-- Maximum amount of tokens in the contract due to transaction limit
tokensLimit :: Integer
tokensLimit = 22
