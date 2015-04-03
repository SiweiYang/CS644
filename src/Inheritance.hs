module Inheritance where

import           Data.List     (intercalate, nub, sort)
import           Data.Map      (Map, fromList, toAscList)
import           Data.Maybe

import           Environment
import           Hierarchy
import           TypeDatabase
import           Util

generateLabelFromFUNC :: Symbol -> Int -> String
generateLabelFromFUNC (FUNC mds ls ln _ _) i = if elem "native" mds
                                                 then case ln of
                                                        "malloc" -> "__malloc"
                                                        "exception" -> "__exception"
                                                        "nativeWrite" -> "NATIVEjava.io.OutputStream.nativeWrite"
                                                 else intercalate "_" (ls ++ [md, ln, show i])
  where
    md = if elem "static" mds
            then "static"
            else "instance"

createTypeID :: TypeNode -> Map Symbol Int
createTypeID db = fromList (zip syms [0..])
  where
    syms = (sort . (map symbol) . dumpDBNodes) db

createInstanceFUNCID :: TypeNode -> Map Symbol Int
createInstanceFUNCID db = fromList (zip syms [0..])
  where
    syms = sort $ filter (\(FUNC mds _ _ _ _) -> not $ elem "static" mds) $ map symbol $ filter isFUNCNode $ concat $ map subNodes (dumpDBNodes db)

createStaticFUNCID :: TypeNode -> Map Symbol Int
createStaticFUNCID db = fromList (zip syms' [0..])
  where
    syms = sort $ filter (\(FUNC mds _ _ _ _) -> elem "static" mds) $ map symbol $ filter isFUNCNode $ concat $ map subNodes (dumpDBNodes db)
    syms' = (symbol runtimeMalloc):syms

createFUNCLabel :: TypeNode -> Map Symbol String
createFUNCLabel db = fromList pairs
  where
    funcMap = createStaticFUNCID db
    pairs = map (\(sym, i) -> (sym, generateLabelFromFUNC sym i)) (toAscList funcMap)

createTypeCharacteristicBM :: TypeNode -> [Bool]
createTypeCharacteristicBM db = map snd smap
  where
    syms = (sort . (map symbol) . dumpDBNodes) db
    smap = [(x * (length syms) + y, isA db (syms !! x) (syms !! y)) | x <- init [0..length syms], y <- init [0..length syms]]

updateFUNC :: Symbol -> [Symbol] -> Maybe Symbol
updateFUNC func imps = case filter (\imp -> funcToSigOrd func == funcToSigOrd imp) imps of
                         [imp] -> Just imp
                         _  -> Nothing

updateFUNCTable :: [Symbol] -> [Symbol] -> [Maybe Symbol]
updateFUNCTable funcs imps = map (\func -> updateFUNC func imps) funcs

createInstanceFUNCTable :: TypeNode -> [(Symbol, [Maybe Symbol])]
createInstanceFUNCTable db = map (\(tp, imps) -> (tp, updateFUNCTable funcs imps)) pairs
  where
    tps = ((sortOn symbol) . (filter isCLNode) . dumpDBNodes) db
    funcsMap = createInstanceFUNCID db
    funcs = map fst (toAscList funcsMap)
    pairs = map (\tp -> (symbol tp, map symbol (filter isFUNCNode $ subNodes tp))) tps

generateEdgeFromPairs :: [(Symbol, [Symbol])] -> [([String], [String])]
generateEdgeFromPairs [] = []
generateEdgeFromPairs ((cSYM, syms):remain) = edges' ++ (generateEdgeFromPairs remain)
  where
    edges = nub [(symbolToCN cSYM, symbolToCN sym) | sym <- syms]
    edges' = filter (\(a, b) -> a /= b) edges

generateClassOrdering :: [[String]] -> [([String], [String])] -> [[String]]
generateClassOrdering [] _ = []
generateClassOrdering nodes edges = case cand of
                                      node:_ -> node:(generateClassOrdering (filter (node /=) nodes) edges')
                                      [] -> error $ "loop: " ++ (show edges)
  where
    edges' = filter (\(e1, e2) -> (elem e1 nodes) || (elem e2 nodes)) edges
    cand = filter (\node -> not $ elem node (map fst edges')) nodes
