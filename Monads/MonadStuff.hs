
module Monads.MonadStuff where

import Control.Monad

forever' :: (Monad m) => m a -> (a -> m a) -> m a
forever' globals f = globals >>= (\v -> forever' (f v) f)

when' :: (Monad m) => Bool -> m a -> a -> m a
when' True m _ = m
when' False _ a = return a
