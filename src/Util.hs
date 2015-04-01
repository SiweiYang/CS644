module Util where

import Data.List

---------------------------------------- SplitOneOf-----------------------------------------------------------------------------
splitOneOf :: (Eq a) => [a] -> [a] -> [[a]]
splitOneOf del (c:r) = h:(splitOneOf del (if t == [] then t else tail t))
    where
        pred = (\x -> elem x del)
        l = c:r
        (h, t) = break pred l
splitOneOf _ [] = []

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn conv l = sortBy (\x y -> compare (conv x) (conv y)) l

indent :: Int -> String -> String
indent i str = intercalate "\n" nlns
    where
        lns = splitOneOf "\n" str
        ind = take i (repeat ' ')
        nlns = map (ind ++) lns
