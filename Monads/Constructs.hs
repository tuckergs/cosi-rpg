
module Monads.Constructs where

import Monads.StateStuff
import Monads.MonadStuff
import Control.Monad.Trans.State.Lazy

startSwitch :: (Monad m) => r -> StateT r m b -> m r
startSwitch defult m = fmap snd $ runStateTF defult m

swCase :: (Monad m) => Bool -> m r -> StateT r m ()
swCase trVal m = onlyState $ when' trVal m

