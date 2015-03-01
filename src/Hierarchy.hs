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
  fmap (++(" in class " ++ name)) (msum $ map (\name -> checkImplementTarget name env) implements)
checkImplementTargets _ _ = Nothing

-- Looks up the interface in the environment
checkImplementTarget :: [String] -> Environment -> HierarchyError
checkImplementTarget name env =
  case findUnitInEnv name Interface env of
    Nothing -> Just $ "interface " ++ last name ++ " does not exist"
    Just (SU _ Class _ _) -> Just $ "Cannot implement class " ++ last name
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
checkExtendTarget _ _ = Nothing

