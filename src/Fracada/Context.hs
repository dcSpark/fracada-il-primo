{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Fracada.Context where

import Ledger                       hiding (txInputs, txOutputs, txMint)
import PlutusPrelude (Generic)
import PlutusTx qualified
import Prelude qualified as Haskell
import PlutusTx.Prelude  as P


data StandardContext = StandardContext
  { info :: !TxInfo,
    txInputs :: ![TxOut],
    txOutputs :: ![TxOut],
    txMint :: !Value
  }
  deriving (Haskell.Show, Generic)

{-# INLINEABLE extractStandardContext #-}
extractStandardContext :: ScriptContext -> StandardContext
extractStandardContext ctx =
  StandardContext
    { txInputs = map txInInfoResolved txInfoInputs,
      txOutputs = txInfoOutputs,
      info = info,
      txMint = txInfoMint info
    }
  where
    info@TxInfo {txInfoInputs, txInfoOutputs} = scriptContextTxInfo ctx

PlutusTx.makeLift ''StandardContext
PlutusTx.makeIsDataIndexed ''StandardContext [('StandardContext, 0)]