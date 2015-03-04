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
  | isJust extendError = extendError
  | otherwise = Nothing
  where implementError = checkImplements unit typeDB
        extendError = checkExtends unit typeDB

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

checkExtends :: CompilationUnit -> TypeNode -> HierarchyError
checkExtends unit@(Comp _ _ (CLS _ clsName (Just extendee) _ _ _ _ _) _) typeDB
  | not extendedNodeExists = Just $ "Class " ++ clsName ++ " tried to extend non-existent class " ++ (show extendee)
  | "final" `elem` (symbolModifiers extendedSymbol) = Just $ "Class " ++ clsName ++ " cannot extend final class " ++ (localName extendedSymbol)
  | extendedName == ownName = Just $ "Class " ++ clsName ++ " cannot extend itself"
  | otherwise = Nothing
  where unitImports = visibleImports unit
        extendedName = traverseTypeEntryWithImports typeDB unitImports extendee
        ownName = traverseTypeEntryWithImports typeDB unitImports [clsName]
        extendedNode = getTypeEntry typeDB (head extendedName)
        extendedNodeExists = isJust extendedNode
        extendedSymbol = symbol . fromJust $ extendedNode
        -- extendedInterfaces = filter isInterface extendedSymbols
checkExtends _ _ = Nothing
