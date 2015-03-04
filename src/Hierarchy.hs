module Hierarchy where

import Control.Monad
import Data.Maybe
import Data.List

import AST
import Environment
import TypeDatabase
import Util

type HierarchyError = Maybe String

checkHierarchies :: [CompilationUnit] -> TypeNode -> HierarchyError
checkHierarchies units typeDB = msum $ map (\unit -> checkHierarchy unit typeDB) units

checkHierarchy :: CompilationUnit -> TypeNode -> HierarchyError
checkHierarchy unit typeDB
  | isJust implementError = implementError
  | otherwise = Nothing
  where implementError = checkImplements unit typeDB

checkImplements :: CompilationUnit -> TypeNode -> HierarchyError
checkImplements unit@(Comp _ _ (CLS _ clsName _ implemented _ _ _ _) _) typeDB
  | not . null $ implementedClasses = Just $ "Class " ++ clsName ++ " cannot implement class " ++ (localName $ head implementedClasses)
  | nub implementedNames /= implementedNames = Just $ "Class " ++ clsName ++ " implements the same interface twice"
  | otherwise = Nothing
  where unitImports = visibleImports unit
        implementedNames = map (traverseTypeEntryWithImports typeDB unitImports) implemented
        implementedNodes = mapMaybe (getTypeEntry typeDB) (map head implementedNames)
        implementedSymbols = map symbol implementedNodes
        implementedClasses = filter isClass implementedSymbols
checkImplements _ _ = Nothing

-- Looks up the interface name, and sees if it exists
checkInterface :: [String] -> [[String]] -> TypeNode -> HierarchyError
checkInterface name imports typeDB =
  case traverseTypeEntryWithImports typeDB imports name of
    [] -> Just $ "interface " ++ last name ++ " does not exist"
    _ -> Nothing
