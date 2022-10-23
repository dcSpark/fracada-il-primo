{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.EvilValidator where
    
import Ledger
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude

{-# INLINEABLE alwaysSucceedsValidator #-}
alwaysSucceedsValidator :: Integer -> () -> ScriptContext -> Bool
alwaysSucceedsValidator _ _ _ =
  True

data AlwaysSucceeding

instance Scripts.ValidatorTypes AlwaysSucceeding where
  type RedeemerType AlwaysSucceeding = ()
  type DatumType AlwaysSucceeding = Integer

alwaysSucceedsTypedValidator :: Scripts.TypedValidator AlwaysSucceeding
alwaysSucceedsTypedValidator =
  Scripts.mkTypedValidator @AlwaysSucceeding
    $$(PlutusTx.compile [||alwaysSucceedsValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Integer @()

alwaysSucceedsValidatorScript :: Validator
alwaysSucceedsValidatorScript = Scripts.validatorScript alwaysSucceedsTypedValidator

alwaysSucceedsValidator' :: Validator
alwaysSucceedsValidator' = Scripts.validatorScript alwaysSucceedsTypedValidator

alwaysSucceedsScrAddress :: Ledger.Address
alwaysSucceedsScrAddress = scriptAddress alwaysSucceedsValidator'