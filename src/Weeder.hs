module Weeder where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List

import AST
import Util

type WeedError = Maybe String

-- Takes a filename and CompilationUnit and returns Just an error message if invalid and Nothing if valid
weed :: String -> CompilationUnit -> WeedError
weed filename unit = weedTypeDec filename $ definition unit

weedTypeDec :: String -> TypeDec -> WeedError
weedTypeDec filename (CLS modifiers name _ _ constructors fields methods info)
  | all (`elem` modifiers) ["abstract", "final"] = Just $ "Cannot have a class that is abstract AND final" ++ show info
  | (nub modifiers) /= modifiers = Just $ "Class modifiers cannot be repeated" ++ show info
  | "private" `elem` modifiers = Just $ "Classes cannot be private" ++ show info
  | not (any (`elem` modifiers) ["public", "protected"]) = Just $ "Class must have acces modifier" ++ show info
  | methodError /= Nothing = methodError
  | fieldError /= Nothing = fieldError
  | constructorError /= Nothing = constructorError
  | name /= correctName = Just $ "Expected class to be named '" ++ correctName ++ "' but saw '" ++ name ++ "'" ++ show info
  | otherwise = Nothing
  where methodError = weedClassMethods methods
        fieldError = weedFields fields
        constructorError = msum $ map weedConstructor constructors
        fileName = last $ splitOneOf "/" filename
        fileNameSplit = splitOneOf "." fileName
        correctName = foldl (++) "" (take ((length fileNameSplit) - 1) fileNameSplit)
weedTypeDec filename (ITF modifiers name _ methods info)
  | "private" `elem` modifiers = Just $ "Interfaces cannot be private" ++ show info
  | (nub modifiers) /= modifiers = Just $ "Interface modifiers cannot be repeated" ++ show info
  | methodError /= Nothing = methodError
  | name /= correctName = Just $ "Expected class to be named '" ++ correctName ++ "' but saw '" ++ name ++ "'" ++ show info
  | otherwise = Nothing
  where methodError = weedInterfaceMethods methods
        correctName = head $ splitOneOf "." (last (splitOneOf "/" filename))

weedConstructor :: Constructor -> WeedError
weedConstructor (Cons modifiers name params invocation definition info)
  | isJust definitionError = definitionError
  | isJust invocationError = invocationError
  | otherwise = Nothing
  where definitionError = weedStatementBlock definition
        invocationError = if isJust invocation then weedExpression $ fromJust invocation else Nothing

weedTypedVar :: TypedVar -> WeedError
weedTypedVar typedVar = Nothing

weedClassMethods :: [Method] -> WeedError
weedClassMethods methods = msum $ map weedClassMethod methods

weedClassMethod :: Method -> WeedError
weedClassMethod (MTD modifiers var params definition info)
  | (nub modifiers) /= modifiers = Just $ "Method modifiers cannot be repeated" ++ show info
  | length (filter (`elem` modifiers) ["public", "protected"]) /= 1 = Just $ "One and only one access modifier is allowed" ++ show info
  | all (`elem` modifiers) ["static", "final"] = Just $ "A static method cannot be final" ++ show info
  | ("native" `elem` modifiers) && not ("static" `elem` modifiers) = Just $ "Native methods must be static" ++ show info
  | "private" `elem` modifiers = Just $ "Methods cannot be private" ++ show info
  | "abstract" `elem` modifiers && "static" `elem` modifiers = Just $ "Abstract methods cannot be static" ++ show info
  | "abstract" `elem` modifiers && "final" `elem` modifiers = Just $ "Abstract methods cannot be final" ++ show info
  | "abstract" `elem` modifiers && "native" `elem` modifiers = Just $ "Abstract methods cannot be native" ++ show info
  | "abstract" `elem` modifiers && "strictfp" `elem` modifiers = Just $ "Abstract methods cannot be strictfp" ++ show info
  | "abstract" `elem` modifiers && "synchronized" `elem` modifiers = Just $ "Abstract methods cannot be synchronized" ++ show info
  | "native" `elem` modifiers && "strictfp" `elem` modifiers = Just $ "Native methods cannot be strictfp" ++ show info
  | any (`elem` modifiers) ["abstract", "native"] && isJust definition = Just $ "Abstract/Native methods cannot have a body" ++ show info
  | not (any (`elem` modifiers) ["abstract", "native"]) && isNothing definition = Just $ "Non-abstract non-native methods must have a body" ++ show info
  | isJust bodyError = bodyError
  | otherwise = Nothing
  where bodyError = weedStatementBlock definition

weedInterfaceMethods :: [Method] -> WeedError
weedInterfaceMethods methods = msum $ map weedInterfaceMethod methods

weedInterfaceMethod :: Method -> WeedError
weedInterfaceMethod (MTD modifiers var params definition info)
  | (nub modifiers) /= modifiers = Just $ "Method modifiers cannot be repeated" ++ show info
  | not (all (`elem` ["public", "abstract"]) modifiers) = Just $ "Interface methods can only have modifiers 'public' and 'abstract'" ++ show info
  | isJust definition = Just $ "Interface methods cannot have a body" ++ show info
  | otherwise = Nothing

weedStatementBlock :: Maybe StatementBlock -> WeedError
weedStatementBlock Nothing = Nothing
weedStatementBlock (Just block) = fmap (++ (show (statementsInfo block))) (msum $ map weedStatement (statements block))

weedStatement :: Statement -> WeedError
weedStatement (LocalVar var value) = weedExpression value
weedStatement (If ifExpression ifBlock elseBlock) = (weedExpression ifExpression)
                                                <|> (weedStatementBlock $ Just ifBlock)
                                                <|> (weedStatementBlock elseBlock)
weedStatement (For initializer expression statement statementBlock)
  | isJust initError = initError
  | isJust exprError = exprError
  | isJust stmtError = stmtError
  where initError = if isJust initializer then weedStatement $ fromJust initializer else Nothing
        exprError = if isJust expression then weedExpression $ fromJust expression else Nothing
        stmtError = if isJust statement then weedStatement $ fromJust statement else Nothing
weedStatement (While expression block)
  | isJust exprError = exprError
  | isJust blockError = blockError
  where exprError = weedExpression expression
        blockError = weedStatementBlock $ Just block
weedStatement (Block statementBlock) = weedStatementBlock $ Just statementBlock
weedStatement (Expr expr) = weedExpression expr
weedStatement (Return exp) = if isJust exp then weedExpression $ fromJust exp else Nothing
weedStatement _ = Nothing

-- TODO: Allow 2147483648 in negation
weedExpression :: Expression -> WeedError
weedExpression (Unary "-" (Value TypeInt value innerDepth) outerDepth)
  | intVal > 2147483648 = Just "Integer literal out of range"
  -- Again, a dirty trick to detect if the integer literal was parenthesized
  | (innerDepth > outerDepth + 11) && intVal > 2147483647 = Just "Integer literal out of range"
  | otherwise = Nothing
  where intVal = read value
weedExpression (Unary operator expression _) = weedExpression expression
weedExpression (Binary operator left right _) = weedExpression left <|> weedExpression right
weedExpression (Attribute expression mem _) = Nothing
weedExpression (ArrayAccess array index _) = weedExpression array <|> weedExpression index
weedExpression (NewArray arrayType dimexprs dims _) = Nothing
weedExpression (Dimension left index _) = Nothing
weedExpression (NewObject classType args _) = Nothing
weedExpression (FunctionCall This _ _) = Just "Cannot call 'this'"
weedExpression (FunctionCall function arguments _) = Nothing
weedExpression (CastA castType dims expression _) = Nothing
weedExpression (CastB (ID _ depthA) expression depthB)
  -- this is a dirty trick to detect invalidly nested expressions since they are rightfully flattened by the AST Builder
  | depthA > depthB + 11 = Just "Invalid cast"
  | otherwise = Nothing
weedExpression (CastB castExpression expression _) = Just "Invalid Cast"
weedExpression (InstanceOf refType expression _) = Nothing
weedExpression (ID identifier _) = Nothing
weedExpression (Value TypeInt value depth)
  | intVal > 2147483647 = Just "Integer literal out of range"
  | otherwise = Nothing
  where intVal = read value
weedExpression (Value valueType value _) = Nothing
weedExpression (This) = Nothing
weedExpression (Null) = Nothing

weedFields :: [Field] -> WeedError
weedFields fields = msum $ map weedField fields

weedField :: Field -> WeedError
weedField (FLD modifiers var val info)
  | (nub modifiers) /= modifiers = Just $ "Field modifiers cannot be repeated" ++ show info
  | "private" `elem` modifiers = Just $ "Class fields cannot be private" ++ show info
  | not (any (`elem` modifiers) ["public", "protected"]) = Just $ "Class field must have access modifier" ++ show info
  | "final" `elem` modifiers && isNothing val = Just $ "Final fields must have values" ++ show info
  | otherwise = Nothing
