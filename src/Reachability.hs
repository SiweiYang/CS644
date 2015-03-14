module Reachability where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List

import AST
import Util

unreachable :: CompilationUnit -> [Statement]
unreachable (Comp _ _ (CLS _ _ _ _ constructors _ methods _) _) =
  let constructorDefinitions = mapMaybe constructorDefinition constructors
      unreachableConstructorStatements = concat $ map (unreachableBlock True) constructorDefinitions
      methodDefinitions = mapMaybe methodDefinition methods
      unreachableMethodStatements = concat $ map (unreachableBlock True) methodDefinitions
  in
    unreachableConstructorStatements ++ unreachableMethodStatements
unreachable _ = []

unreachableBlock :: Bool -> StatementBlock -> [Statement]
unreachableBlock reachable block = unreachableTest reachable $ statements block

-- Returns [] if it can complete normally, or [Statement] if a statemet cannot complete
-- In most cases a statement completes IFF it is reachable
-- The case of checking reachability is the default, and rules are only in place for exceptions to the rule
unreachableTest :: Bool -> [Statement] -> [Statement]
unreachableTest reachable (x:xs) =
  let
    unreachables = case x of
      (Block stmts) -> unreachableBlock reachable stmts
      (Return _) -> xs
      (While expr stmts) -> case conditionConstant expr of
        (Left _) -> unreachableBlock reachable stmts
        (Right True) -> xs
        (Right False) -> x:statements stmts
      (For _ (Just expr) _ stmts) -> case conditionConstant expr of
        (Left _) -> unreachableBlock reachable stmts
        (Right True) -> xs
        (Right False) -> x:statements stmts
      (For _ Nothing _ _) -> xs
      (If _ stmts Nothing) -> unreachableBlock reachable stmts
      (If _ stmts (Just eStmts)) ->
        let trueUnreach = unreachableBlock reachable stmts
            falseUnreach = unreachableBlock reachable eStmts
        in
          if null trueUnreach then []
          else if null falseUnreach then []
          else if length trueUnreach > 0 then trueUnreach
          else falseUnreach
      _ -> if reachable then [] else [x]
    completable = null unreachables
  in
    unreachables ++ (unreachableTest completable xs)
unreachableTest reachable stmts = []


conditionConstant :: Expression -> Either () Bool
conditionConstant (Value _ val _)
  | val == "true" = Right True
  | val == "false" = Right False
  | otherwise = Left ()
conditionConstant (Binary op a b _) =
  let opLeftRight = (op,conditionConstant a,conditionConstant b)
  in case opLeftRight of
    ("&&", Right aVal, Right bVal) -> Right $ aVal && bVal
    ("||", Right aVal, Right bVal) -> Right $ aVal || bVal
    _ -> Left ()
conditionConstant (Unary op expr _) =
  case (op, conditionConstant expr) of
  ("!", Right val) -> Right $ not val
  _ -> Left ()
conditionConstant _ = Left ()
