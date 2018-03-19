
module Monads.ExceptStuff where

import Monads.MonadTransF
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad

toex :: (Monad m) => m a -> ExceptT e m a
toex k  = ExceptT $ fmap Right $ k

brek :: (Monad m) => r -> ExceptT r m a
brek val = ExceptT $ return $ Left val

unleftM :: Monad m => ExceptT r m a -> m r
unleftM em = do
  a <- runExceptT em
  case a of
    Left l -> return l
    Right _ -> error "You just tried to unleft a Right. How could you..."

resetBrek :: Monad m => ExceptT r m a -> ExceptT e m r
resetBrek mt = ExceptT $ do
  res <- runExceptT mt
  case res of
    Left r -> return . Right $ r
    Right _ -> error "You just tried to reset a Right. That\'s wrong!"

instance MonadTransF (ExceptT e) where
  tnest (ExceptT m) = ExceptT $ treturn m
