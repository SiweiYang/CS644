module Reachability where

import Data.Maybe

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
unreachableBlock reachable block = case statements block of
  [(Block sb)] -> unreachableBlock reachable sb
  _ -> unreachableTest reachable $ statements block

-- Returns [] if it can complete normally, or [Statement] if a statemet cannot complete
-- In most cases a statement completes IFF it is reachable
-- The case of checking reachability is the default, and rules are only in place for exceptions to the rule
unreachableTest :: Bool -> [Statement] -> [Statement]
unreachableTest reachable (x:xs) =
  let
    unreachables = case x of
      (Block stmts) -> unreachableBlock reachable stmts
      (Return _) -> [x]
      (While expr stmts) -> case conditionConstant expr of
        (Left _) -> unreachableBlock reachable stmts
        (Right 0) -> x:statements stmts
        (Right _) -> xs
      (For _ (Just expr) _ stmts) -> case conditionConstant expr of
        (Left _) -> unreachableBlock reachable stmts
        (Right 0) -> x:statements stmts
        (Right _) -> xs
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
    willReturn = willComplete [x]
  in
    if willReturn then xs
    else unreachables ++ (unreachableTest completable xs)
unreachableTest reachable stmts = []

-- A non-void function is completable if all execution paths have a return statement
-- All void functions are completable

allCompletable :: CompilationUnit -> Bool
allCompletable(Comp _ _ (CLS _ _ _ _ _ _ methods _) _) =
  let nonVoidMethods = filter (\x -> (typeName . methodVar $ x) /= TypeVoid) methods
      methodDefinitions = mapMaybe methodDefinition nonVoidMethods
      completableMethods = filter canCompleteBlockWithoutReturn methodDefinitions
  in
    (length completableMethods) > 0
allCompletable _ = False

completableBlock :: StatementBlock -> Bool
completableBlock block = willComplete $ statements block

-- True if all execution paths complete, false otherwise
willComplete :: [Statement] -> Bool
willComplete (x:xs) =
  let
    doesComplete = case x of
      (Return _) -> True
      (Block stmts) -> completableBlock stmts
      (If _ stmts (Just eStmts)) ->
        let trueWillComplete = completableBlock stmts
            falseWillComplete = completableBlock eStmts
        in trueWillComplete && falseWillComplete
      _ -> False
  in
    doesComplete || willComplete xs

willComplete [] = False

canCompleteBlockWithoutReturn :: StatementBlock -> Bool
canCompleteBlockWithoutReturn block = canCompleteWithoutReturn $ statements block

canCompleteWithoutReturn :: [Statement] -> Bool
canCompleteWithoutReturn [] = True
canCompleteWithoutReturn ((While expr stmts):xs) = case conditionConstant expr of
  (Left _) -> canCompleteWithoutReturn xs
  (Right 0) -> canCompleteWithoutReturn xs
  (Right _) -> False
canCompleteWithoutReturn ((If _ _ Nothing):xs) = canCompleteWithoutReturn xs
canCompleteWithoutReturn ((Return _):xs) = False
canCompleteWithoutReturn (x:xs) =
  let
    iCanCompleteWithoutReturn = case x of
      (Block stmts) -> canCompleteBlockWithoutReturn stmts
      (If _ stmts (Just eStmts)) ->
        let trueCanCompleteWithoutReturn = canCompleteBlockWithoutReturn stmts
            falseCanCompleteWithoutReturn = canCompleteBlockWithoutReturn eStmts
        in trueCanCompleteWithoutReturn || falseCanCompleteWithoutReturn
      _ -> True
  in
    iCanCompleteWithoutReturn && canCompleteWithoutReturn xs

conditionConstant :: Expression -> Either () Int
conditionConstant (Value TypeByte val _) = Right $ read val
conditionConstant (Value TypeShort val _) = Right $ read val
conditionConstant (Value TypeInt val _) = Right $ read val
conditionConstant (Value _ val _)
  | val == "true" = Right 1
  | val == "false" = Right 0
  | otherwise = Left ()
conditionConstant (Binary op a b _) =
  let opLeftRight = (op,conditionConstant a,conditionConstant b)
  in case opLeftRight of
    ("&&", Right aVal, Right bVal) -> Right $ if aVal == 1 && bVal == 1 then 1 else 0
    ("||", Right aVal, Right bVal) -> Right $ if aVal == 1 || bVal == 1 then 1 else 0
    ("==", Right aVal, Right bVal) -> Right $ if aVal == bVal then 1 else 0
    ("+", Right aVal, Right bVal) -> Right $ aVal + bVal
    ("-", Right aVal, Right bVal) -> Right $ aVal - bVal
    ("*", Right aVal, Right bVal) -> Right $ aVal * bVal
    ("%", Right aVal, Right bVal) -> Right $ aVal `mod` bVal
    _ -> Left ()
conditionConstant (Unary op expr _) =
  case (op, conditionConstant expr) of
  ("!", Right val) -> Right $ if val == 0 then 1 else 0
  ("-", Right val) -> Right $ -val
  _ -> Left ()
conditionConstant _ = Left ()
