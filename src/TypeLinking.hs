module TypeLinking where

import Data.Maybe
import Data.List
import Environment
import TypeDatabase
import AST

typeLinkingCheck :: TypeNode -> [[String]] -> Environment -> [Type]
typeLinkingCheck _ _ ENVE = [TypeVoid]
typeLinkingCheck db imps (ENV su c) = tps
    where
        (SU cname kd st inhf) = su
        [sym] = [sym | sym <- symbolTable inhf, localName sym == last cname]
        SYM mds ln lt = sym
        
        cts = map (\env -> typeLinkingCheck db imps env) c
        cts' = if and $ map (\tps -> tps /= []) cts then [TypeVoid] else []
        
        tps = case kd of
                Var expr -> if typeLinkingExpr db imps su expr /= [] then [TypeVoid] else []
                Exp expr -> typeLinkingExpr db imps su expr
                
                Ret expr -> if typeLinkingExpr db imps su expr == [] then [] else cts'
                WhileBlock expr -> if typeLinkingExpr db imps su expr == [] then [] else cts'
                IfBlock expr -> if typeLinkingExpr db imps su expr == [] then [] else cts'
                
                _ -> cts'

typeLinkingExpr :: TypeNode -> [[String]] -> SemanticUnit -> Expression -> [Type]
typeLinkingExpr db imps su Null = [TypeNull]
typeLinkingExpr db imps su (Unary _ expr _) = typeLinkingExpr db imps su expr
typeLinkingExpr db imps su expr@(Binary op exprL exprR _)
    |   elem op ["+", "-", "*", "/", "%"] = if elem typeL [TypeByte, TypeShort, TypeInt] && elem typeR [TypeByte, TypeShort, TypeInt] then [typeL] else report
    |   elem op ["<", ">", "<=", ">=", "=="] = if elem typeL [TypeByte, TypeShort, TypeInt] && elem typeR [TypeByte, TypeShort, TypeInt] then [TypeBoolean] else report
    |   elem op ["="] = if (typeL == typeR) || typeL == TypeNull || typeR == TypeNull then [TypeInt] else report
	where
		[typeL] = typeLinkingExpr db imps su exprL
		[typeR] = typeLinkingExpr db imps su exprR
                report = (error $ "Binary: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show typeL) ++ (show exprR) ++ (show typeR))
typeLinkingExpr db imps su (ID nm _) = typeLinkingName db imps su nm
typeLinkingExpr db imps su This = [lookUpThis su]
typeLinkingExpr db imps su (Value tp _ _) = [tp]
typeLinkingExpr db imps su (InstanceOf tp expr _) = if typeLinkingExpr db imps su expr == [] then [] else [tp]

typeLinkingExpr db imps su (FunctionCall exprf args _) = case fts of
                                                            [] -> []
                                                            (Function pt rt):_ -> [rt]
                                                            _ -> error ((show fts) ++ (show exprf))
--if and $ map (\(lt, lts) -> [lt] == lts) (zip pt ats) then [rt] else []
        where
                --fts = [Function pt rt | Function pt rt <- typeLinkingExpr db imps su exprf]
                fts = typeLinkingExpr db imps su exprf
                fts' = [Function pt rt | ft@(Function pt rt) <- fts, length pt == length args]
                ats = map (typeLinkingExpr db imps su) args

typeLinkingExpr db imps su (Attribute s m _) = map (symbolToType . symbol) nodes
	where
		[tp] = typeLinkingExpr db imps su s
                --should handle Class and instance differently
                nodes = traverseInstanceEntry' db db ((typeToName tp)++[m])

typeLinkingExpr db imps su (NewObject tp args _) = [Object (Name tpn)]
        where
                [tpn] = traverseTypeEntryWithImports db imps (typeToName tp)
-- to check param types

{-

typeLinkingExpr db imps su (NewArray tp exprd _ _) = if elem typeIdx [TypeByte, TypeShort, TypeInt] then [Array tp] else error "Array: index is not an integer"
        where
                [typeIdx] = typeLinkingExpr db imps su exprd
typeLinkingExpr db imps su (ArrayAccess arr idx _) = case typeArr of
                                                        [tp] -> if elem typeIdx [TypeByte, TypeShort, TypeInt] then [tp] else error "Array: index is not an integer"
                                                        _ -> error "Array Type cannot be found"
	where
		typeArr = typeLinkingExpr db imps su arr 
		[typeIdx] = typeLinkingExpr db imps su idx

-}

{-
                | CastA { casttype :: Type, dims :: Expression, expr :: Expression, depth :: Int }
                | CastB { castexpr :: Expression, expr :: Expression, depth :: Int }
                | CastC { castname :: Name, dims :: Expression, expr :: Expression, depth :: Int }
-}

typeLinkingExpr db imps su _ = [TypeVoid]

{-
Name.String
String --> Type : Object [String] { from DB or Symbol Table }
Object [String] --> a Class --> DB --> next Object
-}

typeLinkingName :: TypeNode -> [[String]] -> SemanticUnit -> Name -> [Type]
typeLinkingName db imps su (Name (nm:remain)) = case syms of
                                                    [] -> lookUpDB db imps (nm:remain)
                                                    _ -> if remain == [] then map symbolToType syms else map (symbolToType . symbol) nodes
	where
		syms = lookUpSymbolTable su nm
                nodes = concat $ map (traverseInstanceEntry' db db) (map (\sym -> (typeToName . localType $ sym) ++ remain) syms)
                

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

symbolToType :: Symbol -> Type
symbolToType (SYM _ _ t) = t
symbolToType (CL _ _ t) = t
symbolToType (IT _ _ t) = t
symbolToType (FUNC _ _ ps rt) = Function ps  rt

lookUpThis :: SemanticUnit -> Type
lookUpThis su = if kind su == Class then Object (Name (scope su)) else lookUpThis (inheritFrom su)

lookUpSymbolTable :: SemanticUnit -> String -> [Symbol]
lookUpSymbolTable (Root _) str = []
lookUpSymbolTable su nm = case cur of
                            [] -> lookUpSymbolTable parent nm
                            _ -> cur
	where
		(SU _ _ st parent) = su
		cur = filter (\s -> (localName s) == nm) st

lookUpDB :: TypeNode -> [[String]] -> [String] -> [Type]
lookUpDB db imps cname = map (symbolToType . symbol) $ nub tps
        where
            ps = map (\i -> (take i cname, drop i cname)) [1..(length cname)]
            tps = concat $ map (\(pre, post) -> traverseInstanceEntry db (traverseFieldEntryWithImports db imps pre) post) ps
            tps' = []