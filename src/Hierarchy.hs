module Hierarchy where

import Control.Monad
import Data.Maybe
import Data.List

import AST
import Environment
import Util

type HierarchyError = Maybe String

checkHierarchies :: [CompilationUnit] -> Environment -> HierarchyError
checkHierarchies units env = msum $ map (\unit -> checkHierarchy unit env) units

checkHierarchy :: CompilationUnit -> Environment -> HierarchyError
checkHierarchy unit env
  | isJust extendError = extendError
  | isJust implementError = implementError
  | otherwise = Nothing
  where extendError = checkExtendTarget (definition unit) env
        implementError = checkImplementTargets (definition unit) env

checkImplementTargets :: TypeDec -> Environment -> HierarchyError
checkImplementTargets (CLS _ name _ implements _ _ _ _) env =
  fmap (++(" in class " ++ name)) (msum $ map (\name -> checkInterface name env) implements)
checkImplementTargets _ _ = Nothing

checkInterface :: [String] -> Environment -> HierarchyError
checkInterface name env =
  case findUnitInEnv name Interface env of
    Nothing -> Just $ "interface " ++ last name ++ " does not exist"
    Just _ -> Nothing

checkClass :: [String] -> Environment -> HierarchyError
checkClass name env =
  case findUnitInEnv name Class env of
    Nothing -> Just $ "interface " ++ last name ++ " does not exist"
    Just _ -> Nothing

checkExtendTarget :: TypeDec -> Environment -> HierarchyError
checkExtendTarget (CLS _ name extends _ _ _ _ _) env =
  if isJust extends then
    case findUnitInEnv (fromJust extends) Class env of
      Nothing -> Just $ name ++ " tried to extend " ++ (last $ fromJust extends) ++ " which does not exist"
      Just (SU _ Interface _ _) -> Just $ name ++ " cannot extend interface " ++ (last $ fromJust extends)
      Just unit -> case getClassSymbol unit (last $ fromJust extends) of
        Nothing -> Just "Couldn't find the symbol for a class, uh-oh!"
        Just symbol ->
          if "final" `elem` (symbolModifiers symbol) then
            Just $ name ++ " cannot extend final class " ++ (last $ fromJust extends)
          else
           Nothing
  else
    Nothing
checkExtendTarget (ITF _ name extends _ _) env
  | name `elem` (map last extends) = Just $ "Interface " ++ name ++ " extends itself"
  | otherwise = fmap (++(" in interface " ++ name)) (msum $ map (\name -> checkInterface name env) extends)

