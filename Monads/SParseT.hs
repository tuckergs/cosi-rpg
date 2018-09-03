
module Monads.SParseT where


import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Monads.ExceptStuff
import Monads.MonadStuff


type SParseT = StateT String


getNextChar :: (Monad m) => SParseT m (Maybe Char)
getNextChar = StateT $ \inp -> case inp of
  [] -> return (Nothing,[])
  (i:is) -> return (Just i,is)

peekNextChar :: (Monad m) => SParseT m (Maybe Char)
peekNextChar = do
  cm <- getNextChar
  case cm of
    Nothing -> return Nothing
    Just c -> StateT $ \lf -> return (Just c,c:lf)

getNextWord :: (Monad m) => SParseT m String
getNextWord = do
  -- Take any spaces before
  unleftM $ forever $ do
    cm <- lift $ peekNextChar
    case cm of
      Nothing -> brek ()
      Just c -> if (c == ' ') 
        then ((lift getNextChar) >> return ())
        else (brek ())
  -- Now get the next word
  unleftM $ forever' (return []) $ 
    \curW -> do
      cm <- lift $ getNextChar
      let res = reverse curW
      case cm of
        Nothing -> brek res
        Just c -> do
          when (c == ' ') $ brek res
          return $ c:curW


-- getNextWord :: (Monad m) => SParseT m a
-- getNextWord = do
  -- case inp of
    -- [] -> return (reverse res,[])
    -- (i:is) -> do
      
getAllWords :: (Monad m) => SParseT m [String]
getAllWords = do
  s <- getNextWord
  ss <- getAllWords
  return $ s:ss
    
  
  

