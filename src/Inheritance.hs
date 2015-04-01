module Inheritance where

import           Data.List    (sort)
import           Data.Map     (Map, fromList)

import           Environment
import           Hierarchy
import           TypeDatabase

createTypeID :: TypeNode -> Map Symbol Int
createTypeID db = fromList (zip syms [0..])
  where
    syms = (sort . (map symbol) . dumpDBNodes) db

createFUNCID :: TypeNode -> Map Symbol Int
createFUNCID db = fromList (zip syms [0..])
  where
    syms = sort $ map symbol $ filter isFUNCNode $ concat $ map subNodes (dumpDBNodes db)


createTypeCharacteristicBM :: TypeNode -> [Bool]
createTypeCharacteristicBM db = map snd smap
  where
    syms = (sort . (map symbol) . dumpDBNodes) db
    smap = [(x * (length syms) + y, isA db (syms !! x) (syms !! y)) | x <- init [0..length syms], y <- init [0..length syms]]
