module Helpers where

iint :: (Ord a,Integral b,RealFrac a) => a -> b
iint a
  | a >= 0 = floor a
  | otherwise = ceiling a

brutalLookup :: Eq a => a -> [(a,b)] -> b
brutalLookup e store
  = case (lookup e store) of
      Just b -> b
      Nothing -> error "brutalLookup failed since element is not in store"

combineMaybeList :: [(Maybe a)] -> [a]
combineMaybeList [] = []
combineMaybeList (Nothing:ls) = combineMaybeList ls
combineMaybeList ((Just x):ls) = x:(combineMaybeList ls)
