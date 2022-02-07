module Fracada.Utils where

import           PlutusTx.Builtins as Builtins
import           PlutusTx.Prelude  as P
import           Prelude           hiding (error)

{-# INLINEABLE fromJust' #-}
fromJust' :: Maybe a -> a
fromJust' (Just a) = a
fromJust' _        = error ()

{-# INLINEABLE fromRight' #-}
fromRight' :: Either b a -> a
fromRight' (Right a) = a
fromRight' _         = error ()
