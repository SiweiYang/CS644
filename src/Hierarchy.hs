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
  where implementError = checkImplementTargets unit typeDB

checkImplementTargets :: CompilationUnit -> TypeNode -> HierarchyError
checkImplementTargets unit typeDB =
  fmap (++(" in class " ++ name)) (msum $ map (\name -> checkInterface name (visibleImports unit) typeDB) implementedInterfaces)
  where
  	implementedInterfaces = implements (definition unit)
  	name = case (definition unit) of
  		(CLS _ clsName _ _ _ _ _ _) -> clsName
  		(ITF _ itfName _ _ _) -> itfName

-- Looks up the interface name, and sees if it exists
checkInterface :: [String] -> [[String]] -> TypeNode -> HierarchyError
checkInterface name imports typeDB =
  case traverseTypeEntryWithImports typeDB imports name of
    Nothing -> Just $ "interface " ++ last name ++ " does not exist"
    Just _ -> Nothing
