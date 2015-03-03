module TypeLinking where

import Data.Maybe
import Data.List
import Environment
import TypeDatabase
import AST

typeLinkingCheck :: TypeNode -> [[String]] -> Environment -> Bool
typeLinkingCheck _ _ ENVE = True
typeLinkingCheck db imps (ENV su c) =
	case su of
			(SU cname kd st inhf) -> -- not use everyone
				case kd of
					Var expr -> isJust $ typeLinkingExpr db imps su expr
					Exp expr -> isJust $ typeLinkingExpr db imps su expr 
					Ret expr -> isJust $ typeLinkingExpr db imps su expr
					WhileBlock expr -> isJust $ typeLinkingExpr db imps su expr
					IfBlock expr ->isJust $ typeLinkingExpr db imps su expr
					_ -> and $ map (typeLinkingCheck db imps) c
			_ -> and $ map (typeLinkingCheck db imps) c

typeLinkingExpr :: TypeNode -> [[String]] -> SemanticUnit -> Expression -> Maybe Type
typeLinkingExpr db imps su Null = Just TypeNull
typeLinkingExpr db imps su (Unary _ expr _) = typeLinkingExpr db imps su expr
typeLinkingExpr db imps su (Binary op exprL exprR _) = if (typeL == typeR) then typeL else (error "Binary: type(left) != type(right)")
	where
		typeL = typeLinkingExpr db imps su exprL
		typeR = typeLinkingExpr db imps su exprR
typeLinkingExpr db imps su (ArrayAccess arr idx _) = case typeArr of
												Array tp -> if elem typeIdx [TypeByte, TypeShort, TypeInt] then Just typeArr else error "Array: index is not an integer"
												_ -> error "Array: not an array"
	where
		typeArr = fromJust $ typeLinkingExpr db imps su arr 
		typeIdx = fromJust $ typeLinkingExpr db imps su idx

typeLinkingExpr db imps su (Value tp _ _) = Just tp

typeLinkingExpr db imps su (Attribute s m _) =
	where
		typeStruct = fromJust $ typeLinkingExpr db su s

typeLinkingExpr db imps su (ID nm _) = fromJust $ typeLinkingName db imps su nm

{-
Name.String
String --> Type : Object [String] { from DB or Symbol Table }
Object [String] --> a Class --> DB --> next Object
-}

typeLinkingName :: TypeNode -> [[String]] -> SemanticUnit -> Name -> Maybe Type
typeLinkingName db imps su (Name nms) =
	where
		

{-
A.B.C.D

objectType :: TypeNode -> [Symbol] -> [[String]] -> String -> Type
objectType db st imps nm = 
	where
		inST = lookUpSymbolTable st nm
		inDB = lookUpTypeDB 

objectTypeNext :: TypeNode -> Name -> String -> Type

lookUpTypeDB :: TypeNode -> [[String]] -> String -> Symbol
lookUpTypeDB st ast 

-}

symbolToType :: Symbol -> Maybe Type
symbolToType (SYM _ _ t) = Just t
symbolToType (FUNC _ _ _ t) = Just t
symbolToType (PKG _) = Nothing
symbolToType (CL _ _) = Nothing

lookUpSymbolTable :: SemanticUnit -> String -> Symbol
lookUpSymbolTable (Root _) str = error (str ++ "is not in the symbol table")
lookUpSymbolTable su nm = if null $ cur then lookUpSymbolTable parent nm else last $ cur
	where
		(SU _ _ st parent) = su
		cur = filter (\s -> (localName s) == nm) st
