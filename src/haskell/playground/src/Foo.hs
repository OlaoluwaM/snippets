module Foo where

import Control.Monad.Except
import Control.Monad.Identity (Identity, IdentityT)
import Control.Monad.State
import Control.Monad.Trans.Maybe

example :: IdentityT (MaybeT (StateT Int Maybe) ) Int
example = do
  n <- get
  put 5
  if n < 0
    then lift $ lift $ lift $ Just 1
    else lift $ lift $ lift $ Just 2
