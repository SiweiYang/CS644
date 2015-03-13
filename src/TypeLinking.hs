module TypeLinking where

import Data.Maybe
import Data.List
import Environment
import TypeDatabase
import AST
import TypeChecking

typeLinkingFailure :: String -> [Type]
--typeLinkingFailure msg = error msg
typeLinkingFailure msg = []

typeLinkingCheck :: TypeNode -> [[String]] -> Environment -> [Type]
typeLinkingCheck _ _ ENVE = [TypeVoid]
typeLinkingCheck db imps (ENV su c) = if elem Nothing imps' then [] else tps
    where
        (SU cname kd st inhf) = su
        (SU cnamei kdi sti inhfi) = inhf
        [sym] = [sym | sym <- symbolTable inhf, localName sym == last cname]
        SYM mds ls ln lt = sym
        
        imps' = map (traverseNodeEntry db) imps
        
        cts = map (\env -> typeLinkingCheck db imps env) c
        cts' = if and $ map (\tps -> tps /= []) cts then [TypeVoid] else []
        
        [varsym] = st
        tps = case kd of
                Var expr -> if typeLinkingExpr db imps su (Binary "=" (ID (Name ([localName varsym])) 0) expr 0) == [] then [] else cts'

                Field varsym (Just expr) -> let syms = dropWhile (varsym /=) [sym | sym@(SYM mds _ _ _) <- sti, not $ elem "static" mds]
                                                forward = or (map (\sym -> forwardSYMInExpr (localName sym) expr) syms)
                                            in if forward
                                                then typeLinkingFailure $ "forward use of syms " ++ (show varsym) ++ (show expr)
                                                else if typeLinkingExpr db imps su (Binary "=" (ID (Name ([localName varsym])) 0) expr 0) == [] then typeLinkingFailure $ "field type mis match expression " ++ (show varsym) ++ (show expr) else cts'
                                                
                Exp expr -> typeLinkingExpr db imps su expr
                
                Ret expr -> let rtp = scopeReturnType su in
                            if rtp == TypeVoid then typeLinkingFailure $ "Return in a void method: " ++ (show expr)
                            else case filter filterNonFunction $ typeLinkingExpr db imps su expr of
                                [] -> typeLinkingFailure $ "Return type no match: " ++ (show expr)
                                [tp] -> if not . null $ assignConversion db tp rtp then [TypeVoid] else typeLinkingFailure $ "Return assign conversion failure: " ++ (show tp) ++ (show rtp)
                                a -> typeLinkingFailure $ "Return type multi match: " ++ (show expr) ++ (show a)

                ForBlock -> let typeCond = cts !! 1
                                casting = castConversion db (head typeCond) TypeBoolean in
                            if (null typeCond) || (null casting) then typeLinkingFailure $ "For condition: " ++ (show typeCond) else cts'


                WhileBlock expr -> let typeExpr = typeLinkingExpr db imps su expr
                                       condition = (not . null $ typeExpr) && (length typeExpr == 1) && (not . null $ castConversion db (head typeExpr) TypeBoolean) in
                                    if condition then cts' else typeLinkingFailure $ "While condition: " ++ (show typeExpr)

                IfBlock expr -> let typeExpr = typeLinkingExpr db imps su expr
                                    condition = (not . null $ typeExpr) && (length typeExpr == 1) && (not . null $ castConversion db (head typeExpr) TypeBoolean) in
                                    if condition then cts' else typeLinkingFailure $ "If condition: " ++ (show typeExpr) ++ (show expr)

                Class -> case typeLinkingPrefix db imps (scope su) of
                            _ -> cts' -- ToDo: double check here!

                Method _ -> cts'

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
typeLinkingExpr db imps su (Unary op expr _) = case op of
                                                "!" -> if null $ castConversion db tp TypeBoolean then typeLinkingFailure "Unary !" else [tp]
                                                "-" -> if null $ castConversion db tp TypeInt then typeLinkingFailure "Unary -" else [tp]
    where
        [tp] = typeLinkingExpr db imps su expr
typeLinkingExpr db imps su expr@(Binary op exprL exprR _)
    |   length typeLs /= 1 || length typeRs /= 1 = []
    |   elem op ["+"] = case (typeL, typeR) of
                            ((Object (Name ["java", "lang", "String"])), _) -> [(Object (Name ["java", "lang", "String"]))]
                            (_, (Object (Name ["java", "lang", "String"]))) -> [(Object (Name ["java", "lang", "String"]))]
                            (_, _) -> if typeLInt && typeRInt then typeLR else typeLinkingFailure "Binary +"
    |   elem op ["-", "*", "/", "%"] = if typeLInt && typeRInt then typeLR else typeLinkingFailure "Binary arithematic op"
    |   elem op ["<", ">", "<=", ">="] = if typeLInt && typeRInt then [TypeBoolean] else typeLinkingFailure "Binary comparision op"
    |   elem op ["&&", "||", "&", "|"] = if typeLBool && typeRBool then [TypeBoolean] else typeLinkingFailure "Binary logical op"
    |   elem op ["==", "!="] = if null equality then typeLinkingFailure $ "Binary " ++ (show typeL) ++ op ++ (show typeR) else [TypeBoolean]
    |   elem op ["="] = if assignRL then [typeL] else typeLinkingFailure $ "Binary =" ++ (show typeL) ++ (show typeR) ++ (show $ assignConversion db typeR typeL)
    where
        typeLs = case filter filterNonFunction $ typeLinkingExpr db imps su exprL of
                            [] -> typeLinkingFailure $ "Binary typeL no match: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show exprR)
                            [l] -> [l]
                            a -> typeLinkingFailure $ "Binary typeL multi match: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show exprR) ++ (show a)
        [typeL] = typeLs
        typeRs = case filter filterNonFunction $ typeLinkingExpr db imps su exprR of
                            [] -> typeLinkingFailure $ "Binary typeR no match: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show exprR)
                            [r] -> [r]
                            a -> typeLinkingFailure $ "Binary typeR multi match: type(left) " ++ op ++ " type(right)" ++ (show exprL) ++ (show exprR) ++ (show a)
        [typeR] = typeRs
        
        assignRL = not . null $ assignConversion db typeR typeL
        typeLR = case (null $ castConversion db typeL typeR, null $ castConversion db typeR typeL) of
                    (True, True) -> []
                    (False, _) -> [typeR]
                    (_, False) -> [typeL]
        typeLInt = not . null $ castConversion db typeL TypeInt
        typeRInt = not . null $ castConversion db typeR TypeInt
        typeLBool= not . null $ castConversion db typeL TypeBoolean
        typeRBool = not . null $ castConversion db typeR TypeBoolean
        equality = equalityCheck db typeL typeR


typeLinkingExpr db imps su (ID nm _) = typeLinkingName db imps su nm
typeLinkingExpr db imps su This = if scopeStatic su then typeLinkingFailure "This not accessible from static scope" else [lookUpThis su]
typeLinkingExpr db imps su (Value tp _ _) = [tp]
--ToDO: check if instance of is legit
typeLinkingExpr db imps su (InstanceOf tp expr _) = let typeExpr = typeLinkingExpr db imps su expr
                                                    in if (isPrimitive tp) || (length typeExpr /= 1) || (isPrimitive (head typeExpr)) then typeLinkingFailure "InstanceOf illegal use" else [TypeBoolean]

typeLinkingExpr db imps su (FunctionCall exprf args _) = if atsFailed then typeLinkingFailure $ "Function types of Args " ++ (show ats) else
                                                            case fts' of
                                                                [] -> typeLinkingFailure ("Function cannot find " ++ (show exprf) ++ (show fts) ++ (show args))
                                                                [(Function nm pt rt)] -> [rt]
                                                                _ -> typeLinkingFailure ("Function find multi " ++ (show exprf) ++ (show args))
--if and $ map (\(lt, lts) -> [lt] == lts) (zip pt ats) then [rt] else []
        where
                --fts = [Function pt rt | Function pt rt <- typeLinkingExpr db imps su exprf]
                ats = map (typeLinkingExpr db imps su) args
                atsFailed = or $ map null ats
                fts = typeLinkingExpr db imps su exprf
                fts' = [Function nm pt rt | ft@(Function nm pt rt) <- fts, argsMatching (concat ats) pt]

typeLinkingExpr db imps su expr@(Attribute s m _) = case typeLinkingExpr db imps su s of
                                                        [] -> []-- typeLinkingFailure ("Attr " ++ (show s) ++ (show m))
                                                        --should handle Class and instance differently
                                                        [tp] -> case traverseInstanceEntry' db db ((typeToName tp)++[m]) of
                                                                    [] -> typeLinkingFailure ("Attr " ++ (show expr) ++ (show ((typeToName tp)++[m])))
                                                                    --instance look up should not return multiple candidates
                                                                    nodes -> map (symbolToType . symbol) nodes
                                                        _ -> typeLinkingFailure ("Attr " ++ (show s) ++ (show m))

-- import rule plays here
typeLinkingExpr db imps su (NewObject tp args dp) = if atsFailed then typeLinkingFailure $ "NewObject types of Args " ++ (show ats) else
                                                    case [TypeClass (Name nm) | TypeClass (Name nm) <- lookUpDB db imps su (typeToName tp)] of
                                                        [] -> typeLinkingFailure $ "New Object []: " ++ (show tp) ++ (show args) ++ (show imps)
                                                        [(TypeClass (Name nm))]-> let Just tn = getTypeEntry db nm
                                                                                      cons = [node | node@(TN (FUNC mds _ _ pt _) _) <- subNodes tn, elem "cons" mds, argsMatching (concat ats) pt]
                                                                                   in if elem "abstract" ((symbolModifiers . symbol) tn)
                                                                                        then typeLinkingFailure $ "New Object: cannot create abstract class object" ++ (show nm)
                                                                                        else if cons == []
                                                                                                then typeLinkingFailure $ "New Object: no matching constructor for " ++ (show ats) ++ (show [node | node@(TN (FUNC mds _ _ pt _) _) <- subNodes tn, elem "cons" mds])
                                                                                                else [Object (Name nm)]
                                                        tcs -> typeLinkingFailure $ "New Object multi: " ++ (show tcs) ++ (show tp) ++ (show args)
    where
        ats = map (typeLinkingExpr db imps su) args
        atsFailed = or $ map null ats
-- to check param types

typeLinkingExpr db imps su (NewArray tp exprd _ _) = if (not . null $ typeIdx) && (not . null $ castConversion db (head typeIdx) TypeInt)
                                                        then if isPrimitive tp then [Array tp] else
                                                            case [Object (Name nm) | TypeClass (Name nm) <- lookUpDB db imps su (typeToName tp)] of
                                                            [] -> typeLinkingFailure $ "NewArray: []" ++ (show tp) ++ (show $ lookUpDB db imps su (typeToName tp))
                                                            [tp] -> [Array tp]
                                                            tps' -> typeLinkingFailure (show tps')
                                                        else typeLinkingFailure "Array: index is not an integer"
    where
        typeIdx = case typeLinkingExpr db imps su exprd of
                        [] -> typeLinkingFailure $ "Array Index type []: " ++ (show exprd)
                        [tp] -> [tp]
                        tps -> typeLinkingFailure $ "Array Index multi: " ++ (show exprd) ++ (show tps)

typeLinkingExpr db imps su (Dimension _ expr _) = case typeIdx of
                                                        [tp] -> if elem tp [TypeByte, TypeShort, TypeInt, TypeChar] then [tp] else typeLinkingFailure "Array: index is not an integer"
                                                        _ -> typeLinkingFailure "Array Index Type typeLinkingFailure"
    where
        typeIdx = typeLinkingExpr db imps su expr

typeLinkingExpr db imps su (ArrayAccess arr idx _) = case typeArr of
                                                        [Array tp] -> case typeIdx of
                                                                    [tp'] -> if elem tp' [TypeByte, TypeShort, TypeInt] then [tp] else typeLinkingFailure "Array: index is not an integer"
                                                                    _ -> typeLinkingFailure "Array Index Type typeLinkingFailure"
                                                        _ -> typeLinkingFailure "Array Type cannot be found"
    where
        typeArr = typeLinkingExpr db imps su arr 
        typeIdx = typeLinkingExpr db imps su idx
                
                
-- to check: allow use array of primitive type to cast
typeLinkingExpr db imps su (CastA casttp dim expr _) = case typeExpr of
                                                        [] -> typeLinkingFailure $ "CastA: cannot type linking the expression " ++ (show expr)
                                                        [tp] -> if null $ castConversion db tp targetType then typeLinkingFailure $ "CastA: cannot make the cast " ++ (show tp) ++ (show targetType) else [targetType]
                                                        tps -> typeLinkingFailure $ "CastA: expression have multi types " ++ (show expr) ++ (show tps)
        where
            typeExpr = typeLinkingExpr db imps su expr
            targetType = if dim == Null then casttp else (Array casttp)

-- to do: is it possible cast from A to B?
typeLinkingExpr db imps su (CastB castexpr expr _) = if null typeCastExpr || null typeExpr || null casting then typeLinkingFailure "CastB: cannot type linking the expression" else typeCastExpr
        where
            typeCastExpr = case typeLinkingExpr db imps su castexpr of
                            [] -> typeLinkingFailure $ "CastB CastExpr no match " ++ (show castexpr)
                            [TypeClass tp] -> [Object tp]
                            [tp] -> [tp]
                            tps -> typeLinkingFailure $ "CastB CastExpr multi match " ++ (show castexpr) ++ (show tps) ++ (show db)
            typeExpr = case typeLinkingExpr db imps su expr of
                            [] -> typeLinkingFailure $ "CastB Expr no match " ++ (show expr) 
                            [tp] -> [tp]
                            tps -> typeLinkingFailure $ "CastB Expr multi match " ++ (show expr) ++ (show tps) ++ (show db)
            casting = case castConversion db (head typeExpr) (head typeCastExpr) of
                            [] -> typeLinkingFailure $ "CastB Casting " ++ (show typeExpr) ++ (show typeCastExpr)
                            tps -> tps

-- to check: must be (Name [])?
typeLinkingExpr db imps su (CastC castnm _ expr _) = if null tps || null typeExpr || null casting then typeLinkingFailure $ "CastC: cannot type linking the expression" ++ (show expr) ++ (show tps) else [targetType]
        where
            tps = case typeLinkingName db imps su castnm of
                    [] -> typeLinkingFailure "CastC no match"
                    [TypeClass tp] -> [Object tp]
                    [tp] -> [tp]
                    tpss -> typeLinkingFailure $ "CastC multi match: " ++ (show tpss)
            typeExpr = typeLinkingExpr db imps su expr
            targetType = Array (head tps) -- BUG?
            casting = castConversion db (head typeExpr) targetType

typeLinkingExpr db imps su _ = [TypeVoid]

---------------------------------------------------------------------------------------------------------

argsMatching :: [Type] -> [Type] -> Bool
argsMatching x y
    | length x /= length y = False
    | (null x) && (null y) = True
    | casting == False = False
    | otherwise = argsMatching (tail x) (tail y)
    where
        xHead = head x
        yHead = head y
        casting = case (isPrimitive xHead, isPrimitive yHead) of
                    (True, False) -> (boxingType xHead) == yHead
                    (False, True) -> (boxingType yHead) == xHead
                    _ -> xHead == yHead

---------------------------------------------------------------------------------------------------------

typeLinkingName :: TypeNode -> [[String]] -> SemanticUnit -> Name -> [Type]
typeLinkingName db imps su (Name cname@(nm:remain)) = case typeLinkingName' db imps su (Name cname) of
                                                        [] -> typeLinkingFailure $ "Link Name failure: " ++ (show cname)
                                                        tps -> tps

typeLinkingName' :: TypeNode -> [[String]] -> SemanticUnit -> Name -> [Type]
typeLinkingName' db imps su (Name cname@(nm:remain)) = case syms'' of
                                                        [] -> case symsInheritance''' of
                                                                [] -> lookUpDB db imps su (nm:remain)
                                                                _ -> map symbolToType symsInheritance''
                                                        _ -> map symbolToType syms''
	where
                baseName = (typeToName . lookUpThis) su
		syms = [sym | sym <- lookUpSymbolTable su nm, not $ elem "cons" (symbolModifiers sym)]
                symsLocal = [sym | sym@(SYM mds scope _ _) <- syms, (scope /= baseName)]
                symsStatic = symsLocal ++ [sym | sym@(SYM mds scope _ _) <- syms, elem "static" mds] ++ [func | func@(FUNC mds _ _ _ _) <- syms, elem "static" mds]
                symsNStatic = symsLocal ++ [sym | sym <- syms, not $ elem sym symsStatic]
                syms' = if scopeStatic su then symsStatic else symsNStatic
                syms'' = if remain == []
                            then syms'
                            else map symbol $ concat $ map (traverseInstanceEntry' db db) [((typeToName . localType) sym) ++ remain | sym@(SYM mds _ _ _) <- syms']
                
                Just thisNode = getTypeEntry db (typeToName . lookUpThis $ su)
                symsInheritance = [sym | sym <- map symbol (traverseInstanceEntry' db thisNode [nm]), not $ elem "cons" (symbolModifiers sym)]
                symsInheritanceStatic = [sym | sym@(SYM mds _ _ _) <- symsInheritance, elem "static" mds] ++ [func | func@(FUNC mds _ _ _ _) <- symsInheritance, elem "static" mds]
                symsInheritanceNStatic = [sym | sym <- symsInheritance, not $ elem sym symsInheritanceStatic]
                symsInheritance' = if scopeStatic su then symsInheritanceStatic else symsInheritanceNStatic
                symsInheritance'' = if remain == []
                            then symsInheritance'
                            else map symbol $ concat $ map (traverseInstanceEntry' db db) [((typeToName . localType) sym) ++ remain | sym@(SYM mds _ _ _) <- symsInheritance']
                symsInheritance''' = if scopeLocal su then symsInheritance'' else []

---------------------------------------------------------------------------------------------------------

lookUpThis :: SemanticUnit -> Type
lookUpThis su = if elem (kind su) [Class, Interface] then Object (Name (scope su)) else lookUpThis (inheritFrom su)

scopeStatic:: SemanticUnit -> Bool
scopeStatic su
    | elem kd [Package, Class, Interface] = True
    | otherwise = rst
    where
        kd = kind su
        rst = case kd of
                Method (FUNC mds _ _ _ _) -> elem "static" mds
                Field (SYM mds _ _ _) _ -> elem "static" mds
                _ -> scopeStatic (inheritFrom su)

scopeLocal:: SemanticUnit -> Bool
scopeLocal su
    | elem kd [Package, Class, Interface] = False
    | otherwise = rst
    where
        kd = kind su
        rst = case kd of
                Method (FUNC mds _ _ _ _) -> True
                _ -> scopeLocal (inheritFrom su)

scopeReturnType:: SemanticUnit -> Type
scopeReturnType su = rst
    where
        kd = kind su
        rst = case kd of
                Method (FUNC mds _ _ _ lt) -> lt
                _ -> scopeReturnType (inheritFrom su)

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
checkSameNameUp su@(SU _ kd st parent) accst = case kd of
                                                Method _ -> res || checkSameNameUp parent []
                                                Interface -> res || checkSameNameUp parent []
                                                Class -> res || checkSameNameUp parent []
                                                _ -> checkSameNameUp parent nextst
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

------------------------------------------------------------------------------
forwardSYMInExpr :: String -> Expression -> Bool
forwardSYMInExpr nm (Unary op expr _) = forwardSYMInExpr nm expr
forwardSYMInExpr nm expr@(Binary op exprL exprR _)
    |   elem op ["="] = case exprL of
                            ID exprL' _ -> forwardSYMInExpr nm exprR
                            _ -> or [forwardSYMInExpr nm exprL, forwardSYMInExpr nm exprR]
    |   otherwise = or [forwardSYMInExpr nm exprL, forwardSYMInExpr nm exprR]
forwardSYMInExpr nm (ID (Name cname) _) = nm == head cname
forwardSYMInExpr nm This = False
forwardSYMInExpr nm (Value tp _ _) = False
forwardSYMInExpr nm (InstanceOf tp expr _) = forwardSYMInExpr nm expr
forwardSYMInExpr nm (FunctionCall exprf args _) = or ((forwardSYMInExpr nm exprf):(map (forwardSYMInExpr nm) args))
forwardSYMInExpr nm expr@(Attribute s m _) = forwardSYMInExpr nm s
forwardSYMInExpr nm (NewObject tp args dp) = or (map (forwardSYMInExpr nm) args)
forwardSYMInExpr nm (NewArray tp expr _ _) = forwardSYMInExpr nm expr
forwardSYMInExpr nm (Dimension _ exprd _) = forwardSYMInExpr nm exprd
forwardSYMInExpr nm (ArrayAccess arr idx _) = or [forwardSYMInExpr nm arr, forwardSYMInExpr nm idx]
forwardSYMInExpr nm (CastA casttp dim expr _) = forwardSYMInExpr nm expr
forwardSYMInExpr nm (CastB castexpr expr _) = forwardSYMInExpr nm expr
forwardSYMInExpr nm (CastC castnm _ expr _) = forwardSYMInExpr nm expr
forwardSYMInExpr nm _ = False
