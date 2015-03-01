module Hierarchy where

import Control.Monad
import Data.Maybe
import Data.List

import AST
import Environment
import Util

type HierarchyError = Maybe String

checkHierarchies :: [CompilationUnit] -> [Environment] -> HierarchyError
checkHierarchies units envs = msum $ map (\unit -> checkHierarchy unit envs) units

checkHierarchy :: CompilationUnit -> [Environment] -> HierarchyError
checkHierarchy unit envs = checkExtendTarget (definition unit) envs

checkExtendTarget :: TypeDec -> [Environment] -> HierarchyError
checkExtendTarget (CLS _ name extends implements _ _ _ _) envs =
  if isJust extends then
    case findUnitInEnvs (fromJust extends) envs of
      Just (SU _ Interface _ _) -> Just $ name ++ " cannot extend interface " ++ (last $ fromJust extends)
      Just parent -> Nothing
      Nothing -> Just $ name ++ " tried to extend " ++ (last $ fromJust extends) ++ " which does not exist"
  else
    Nothing
checkExtendTarget _ _ = Nothing

findUnitInEnvs :: [String] -> [Environment] -> Maybe SemanticUnit
findUnitInEnvs name envs = msum $ map (findUnitInEnv name) envs

findUnitInEnv :: [String] -> Environment -> Maybe SemanticUnit
findUnitInEnv _ ENVE = Nothing
findUnitInEnv name (ENV unit children)
  | last (scope unit) == last name = Just unit
  | otherwise = Nothing

