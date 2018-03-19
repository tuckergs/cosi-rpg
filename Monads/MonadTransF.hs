
module Monads.MonadTransF where

import Control.Monad
import Control.Monad.Trans.Class


class MonadTrans t => MonadTransF t where
  tnest :: (Monad m, MonadTrans s) => t m a -> t (s m) a
  -- tmap :: (Monad m, Monad n) => (m a -> n b) -> t m a -> t n b


treturn :: (Monad m, MonadTrans t) => m a -> t m a
treturn = lift
