
module Monads.StateStuff where

import Control.Monad.State.Lazy
import Control.Monad
import Control.Monad.Trans.Class
import Monads.MonadTransF

tos :: (Monad m) => m a -> StateT s m a
tos m = StateT $ \s -> fmap (\a -> (a,s)) m

-- execStateT :: (Monad m) => s -> StateT s m a -> m (a,s)
-- execStateT st action = runStateT action st

instance MonadTransF (StateT s) where
  tnest (StateT m) = StateT $ \s -> lift (m s)


onlyState :: Monad m => (s -> m s) -> StateT s m ()
onlyState k = StateT $ \s -> fmap (\x -> ((),x)) (k s)

onlyAction :: Monad m => (s -> m a) -> StateT s m a
onlyAction k = StateT $ \s -> fmap (\a -> (a,s)) (k s)

runStateTF :: Monad m => s -> StateT s m a -> m (a,s)
runStateTF = flip runStateT

  -- tmap f (StateT m) = StateT $ \s -> do
    -- b <- f $ (fmap $ \(a,s) -> a) $ k s
    -- return (b,s)
