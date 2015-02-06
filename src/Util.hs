module Util where

---------------------------------------- SplitOneOf-----------------------------------------------------------------------------
splitOneOf :: (Eq a) => [a] -> [a] -> [[a]]
splitOneOf del (c:r) = h:(splitOneOf del (if t == [] then t else tail t))
    where
        pred = (\x -> elem x del)
        l = c:r
        (h, t) = break pred l
splitOneOf _ [] = []
