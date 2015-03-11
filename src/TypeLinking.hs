module TypeLinking where

import Data.Maybe
import Data.List
import Environment
import TypeDatabase
import AST
import TypeChecking

typeLinkingCheck :: TypeNode -> [[String]] -> Environment -> [Type]
typeLinkingCheck _ _ ENVE = [TypeVoid]
typeLinkingCheck db imps (ENV su c) = if elem Nothing imps' then [] else tps
    where
        (SU cname kd st inhf) = su
        [sym] = [sym | sym <- symbolTable inhf, localName sym == last cname]
        SYM mds ls ln lt = sym
        
        imps' = map (traverseNodeEntry db) imps
        
        cts = map (\env -> typeLinkingCheck db imps env) c
        cts' = if and $ map (\tps -> tps /= []) cts then [TypeVoid] else []
        
        tps = case kd of
                Var expr -> if typeLinkingExpr db imps su expr /= [] then [TypeVoid] else []
                Exp expr -> typeLinkingExpr db imps su expr
                
                Ret expr -> if typeLinkingExpr db imps su expr == [] then [] else cts'
                WhileBlock expr -> if typeLinkingExpr db imps su expr == [] then [] else cts'
                IfBlock expr -> if typeLinkingExpr db imps su expr == [] then [] else cts'
                
                Class -> case typeLinkingPrefix db imps (scope su) of
                            _ -> cts'
                Method -> cts'
                _ -> cts'

typeLinkingPrefix :: TypeNode -> [[String]] -> [String] -> [[String]]
typeLinkingPrefix db imps cname = concat tps
    where
        ps = map (\i -> (take i cname)) [1..(length $ init cname)]
        tps = map (traverseTypeEntryWithImports db imps) ps


filterNonFunction (Function _ _ _) = False
filterNonFunction _ = True
---------------------------------------------------------------------------------------------------------

typeLinkingExpr :: TypeNode -> [[String]] -> SemanticUnit -> Expression -> [Type]
typeLinkingExpr db imps su Null = [TypeNull]
typeLinkingExpr db imps su (Unary _ expr _) = typeLinkingExpr db imps su expr
typeLinkingExpr db imps su expr@(Binary op exprL exprR _)
    |   elem op ["+"] = case (typeL, typeR) of
                            ((Object (Name ["java", "lang", "String"])), _) -> [(Object (Name ["java", "lang", "String"]))]
                            (_, (Object (Name ["java", "lang", "String"]))) -> [(Object (Name ["java", "lang", "String"]))]
                            (_, _) -> if typeLInt && typeRInt then typeLR else []
    |   elem op ["-", "*", "/", "%"] = if typeLInt && typeRInt then typeLR else []
    |   elem op ["<", ">", "<=", ">="] = if typeLInt && typeRInt then [TypeBoolean] else []
    |   elem op ["&&", "||"] = if typeLBool && typeRBool then [TypeBoolean] else []
    |   elem op ["=="] = if null typeLR then [] else [TypeBoolean]
    |   elem op ["="] = if assignRL then [typeL] else []
    where
        [typeL] = case filter filterNonFunction $ typeLinkingExpr db imps su exprL of
                            [] -> error $ "Binary: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show typeL) ++ (show exprR)
                            [l] -> [l]
                            a -> error $ "Binary: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show exprR) ++ (show a)

        [typeR] = case filter filterNonFunction $ typeLinkingExpr db imps su exprR of
                            [] -> error $ "Binary: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show typeL) ++ (show exprR)
                            [r] -> [r]
                            a -> error $ "Binary: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show typeL) ++ (show exprR) ++ (show a)
        report = (error $ "Binary: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show typeL) ++ (show exprR) ++ (show typeR))
        assignRL = not . null $ conversion db typeR typeL
        typeLR = case (null $ conversion db typeL typeR, null $ conversion db typeR typeL) of
                    (True, True) -> []
                    (False, _) -> [typeR]
                    (_, False) -> [typeL]
        typeLInt = not . null $ conversion db typeL TypeInt
        typeRInt = not . null $ conversion db typeR TypeInt
        typeLBool= not . null $ conversion db typeL TypeBoolean
        typeRBool = not . null $ conversion db typeR TypeBoolean


typeLinkingExpr db imps su (ID nm _) = typeLinkingName db imps su nm
typeLinkingExpr db imps su This = [lookUpThis su]
typeLinkingExpr db imps su (Value tp _ _) = [tp]
typeLinkingExpr db imps su (InstanceOf tp expr _) = if typeLinkingExpr db imps su expr == [] then [] else [tp]

typeLinkingExpr db imps su (FunctionCall exprf args _) = case fts' of
                                                            [] -> []--error ("func " ++ (show exprf) ++ (show fts) ++ (show args))
                                                            (Function nm pt rt):_ -> [rt]
                                                            _ -> error ("func mul" ++ (show exprf) ++ (show args))
--if and $ map (\(lt, lts) -> [lt] == lts) (zip pt ats) then [rt] else []
        where
                --fts = [Function pt rt | Function pt rt <- typeLinkingExpr db imps su exprf]
                fts = typeLinkingExpr db imps su exprf
                fts' = [Function nm pt rt | ft@(Function nm pt rt) <- fts, length pt == length args]
                ats = map (typeLinkingExpr db imps su) args

typeLinkingExpr db imps su expr@(Attribute s m _) = case typeLinkingExpr db imps su s of
                                                        [] -> []-- error ("Attr " ++ (show s) ++ (show m))
                                                        --should handle Class and instance differently
                                                        [tp] -> case traverseInstanceEntry' db db ((typeToName tp)++[m]) of
                                                                    [] -> error ("Attr " ++ (show expr) ++ (show ((typeToName tp)++[m])))
                                                                    --instance look up should not return multiple candidates
                                                                    nodes -> map (symbolToType . symbol) nodes
                                                        _ -> []-- error ("Attr " ++ (show s) ++ (show m))

-- import rule plays here
typeLinkingExpr db imps su (NewObject tp args dp) = case [TypeClass (Name nm) | TypeClass (Name nm) <- lookUpDB db imps su (typeToName tp)] of
                                                        [] -> [] --error $ "New Object: " ++ (show tp) ++ (show args) ++ (show imps)
                                                        (TypeClass (Name nm)):_ -> [Object (Name nm)]--let cname = typeToName tp' in map (symbolToType . symbol) (traverseFieldEntryWithImports db imps (cname ++ [last cname]))
-- to check param types

typeLinkingExpr db imps su (NewArray tp exprd _ _) = if elem typeIdx [TypeByte, TypeShort, TypeInt] then [Array tp] else error "Array: index is not an integer"
        where
                [typeIdx] = typeLinkingExpr db imps su exprd

typeLinkingExpr db imps su (Dimension _ expr _) = case typeIdx of
                                                        [tp] -> if elem tp [TypeByte, TypeShort, TypeInt] then [tp] else []--error "Array: index is not an integer"
                                                        _ -> []--error "Array Index Type error"
        where
                typeIdx = typeLinkingExpr db imps su expr

typeLinkingExpr db imps su (ArrayAccess arr idx _) = case typeArr of
                                                        [tp] -> case typeIdx of
                                                                    [tp'] -> if elem tp' [TypeByte, TypeShort, TypeInt] then [tp] else error "Array: index is not an integer"
                                                                    _ -> []--error "Array Index Type error"
                                                        _ -> []--error "Array Type cannot be found"
    where
        typeArr = typeLinkingExpr db imps su arr 
        typeIdx = typeLinkingExpr db imps su idx
                
                
-- to check: allow use array of primitive type to cast
typeLinkingExpr db imps su (CastA casttp dim expr _) = case typeExpr of
                                                        [] -> []--error "CastA: cannot type linking the expression"
                                                        _ -> if dim == Null then [casttp] else [Array casttp]
        where
            typeExpr = typeLinkingExpr db imps su expr

-- to do: is it possible cast from A to B?
typeLinkingExpr db imps su (CastB castexpr expr _) = case typeExpr of
                                                        [] -> []--error "CastB: cannot type linking the expression"
                                                        _ -> case typeCastExpr of
                                                                [] -> []--error "CastB: cannot type linking the cast expression"
                                                                _ -> typeCastExpr
        where
            typeCastExpr = typeLinkingExpr db imps su castexpr
            typeExpr = typeLinkingExpr db imps su expr

-- to check: must be (Name [])?
typeLinkingExpr db imps su (CastC castnm _ expr _) = case typeExpr of
                                                        [] -> []--error "CastC: cannot type linking the expression"
                                                        _-> case tps of
                                                                [] -> []
                                                                tp:_ -> [Array tp]
        where
            tps = typeLinkingName db imps su castnm
            typeExpr = typeLinkingExpr db imps su expr
            

typeLinkingExpr db imps su _ = [TypeVoid]

---------------------------------------------------------------------------------------------------------

typeLinkingName :: TypeNode -> [[String]] -> SemanticUnit -> Name -> [Type]
typeLinkingName db imps su (Name cname@(nm:remain)) = case syms' of
                                                                    [] -> case withThis of
                                                                            [] -> lookUpDB db imps su (nm:remain)
                                                                            _ -> map (symbolToType . symbol) withThis
                                                                    _ -> syms'
    where
        syms = lookUpSymbolTable su nm
        syms' = if remain == [] then map symbolToType syms else map (symbolToType . symbol) nodes
                
        nodes = concat $ map (traverseInstanceEntry' db db) (map (\sym -> (typeToName . localType $ sym) ++ remain) syms)
                
        Just thisNode = getTypeEntry db (typeToName . lookUpThis $ su)
        withThis = traverseInstanceEntry' db thisNode cname

---------------------------------------------------------------------------------------------------------

lookUpThis :: SemanticUnit -> Type
lookUpThis su = if elem (kind su) [Class, Interface] then Object (Name (scope su)) else lookUpThis (inheritFrom su)

lookUpSymbolTable :: SemanticUnit -> String -> [Symbol]
lookUpSymbolTable (Root _) str = []
lookUpSymbolTable su nm = case cur of
                            [] -> lookUpSymbolTable parent nm
                            _ -> cur
    where
        (SU _ _ st parent) = su
        cur = filter (\s -> (localName s) == nm) st

lookUpDB :: TypeNode -> [[String]] -> SemanticUnit -> [String] -> [Type]
lookUpDB db imps su cname
    | length tps' > 0 = tps'
    | otherwise = (map (symbolToType . symbol) $ nub tps)
        where
            ps = map (\i -> (take i cname, drop i cname)) [1..(length cname)]
            tps = concat $ map (\(pre, post) -> traverseInstanceEntry db (traverseFieldEntryWithImports db imps pre) post) ps
            tps' = map (TypeClass . Name) (lookUpType db imps cname)

------------------------------------------------------------------------------------

checkSameNameInEnvironment :: Environment -> Bool
checkSameNameInEnvironment ENVE = False
checkSameNameInEnvironment (ENV su [ENVE]) = checkSameNameUp su []
checkSameNameInEnvironment (ENV su []) = checkSameNameUp su []
checkSameNameInEnvironment (ENV su chs) = or $ map checkSameNameInEnvironment chs


checkSameNameUp :: SemanticUnit -> [Symbol] -> Bool
checkSameNameUp (Root _) accst = checkSameNameInSymbolTable accst
checkSameNameUp su@(SU _ kd st parent) accst = if kd `elem` [Method, Interface, Class] then (res || checkSameNameUp parent []) else checkSameNameUp parent nextst
    where
        nextst = accst ++ st
        res = functionCheck || checkSameNameInSymbolTable nextst
        
        functionCheck = length cons /= (length . nub) cons || length funcs /= (length . nub) funcs
        cname = lookUpThis su
        cons = [(ln, pt) | f@(FUNC mds _ ln pt lt) <- st, elem "cons" mds]
        funcs = [(ln, pt) | f@(FUNC mds _ ln pt lt) <- st, not $ elem "cons" mds]


checkSameNameInSymbolTable :: [Symbol] -> Bool
checkSameNameInSymbolTable st = length nms /= (length . nub) nms
    where
        syms = [SYM mds ls nm tp | SYM mds ls nm tp <- st]
        nms = map localName syms
