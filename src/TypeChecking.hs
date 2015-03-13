module TypeChecking where

import Data.List

import AST
import Environment
import TypeDatabase
import Hierarchy
import Data.Maybe

assignConversion :: TypeNode -> Type -> Type -> [Type]
assignConversion typeDB typeS typeT
    | typeS == typeT = [typeT]
    | typeT == TypeNull = []
    | typeS == TypeNull = case typeT of
                            (Object x) -> [(Object x)]
                            (Array x) -> [(Array x)]
                            _ -> []
    | otherwise = case (isPrimitive typeS, isPrimitive typeT) of
                            (True, True) ->  if null (primitiveConversionA typeS typeT) then [] else typeS:(primitiveConversionA typeS typeT)
                            (False, True) -> case unboxed of
                                                Nothing -> []
                                                Just t -> if t == typeT then [typeS, typeT] else (if null nextUnbox then [] else [typeS, t] ++ nextUnbox)
                            (False, False) -> if null (objectConversionA typeDB typeS typeT) then [] else typeS:(objectConversionA typeDB typeS typeT)
                            (True, False) -> case (boxed == typeT) of
                                                True -> [typeS, typeT]
                                                False -> if null nextBox then [] else [typeS, boxed] ++ nextBox
    where
        boxed = boxingType typeS
        nextBox = objectConversionA typeDB boxed typeT
        unboxed = unboxingType typeS
        nextUnbox = primitiveConversionA (fromJust unboxed) typeT


castConversion :: TypeNode -> Type -> Type -> [Type]
castConversion typeDB typeS typeT
    | typeS == typeT = [typeT]
    | typeT == TypeNull = []
    | typeS == TypeNull = case typeT of
                            (Object x) -> [(Object x)]
                            (Array x) -> [(Array x)]
                            _ -> []
    | typeT == (Object (Name ["java", "lang", "String"])) = [(Object (Name ["java", "lang", "String"]))]
    | otherwise = case (isPrimitive typeS, isPrimitive typeT) of
                            (True, True) ->  if null (primitiveConversionB typeS typeT) then [] else typeS:(primitiveConversionB typeS typeT)
                            (False, True) -> case unboxed of
                                                Nothing -> []
                                                Just t -> if t == typeT then [typeS, typeT] else (if null nextUnbox then [] else [typeS, t] ++ nextUnbox)
                            (False, False) -> if null (objectConversionB typeDB typeS typeT) then [] else typeS:(objectConversionB typeDB typeS typeT)
                            (True, False) -> case (boxed == typeT) of
                                                True -> [typeS, typeT]
                                                False -> if null nextBox then [] else [typeS, boxed] ++ nextBox
    where
        boxed = boxingType typeS
        nextBox = objectConversionB typeDB boxed typeT
        unboxed = unboxingType typeS
        nextUnbox = primitiveConversionB (fromJust unboxed) typeT


----------------------------------------------------------

-- for assignment
primitiveConversionA :: Type -> Type -> [Type]
primitiveConversionA TypeBoolean _ = []
primitiveConversionA TypeByte TypeShort = [TypeShort]
primitiveConversionA TypeByte TypeInt = [TypeInt]
primitiveConversionA TypeShort TypeInt = [TypeInt]
primitiveConversionA TypeChar TypeInt = [TypeInt]
primitiveConversionA _ _ = []

-- for casting
primitiveConversionB :: Type -> Type -> [Type]
primitiveConversionB TypeBoolean TypeBoolean = [TypeBoolean]
primitiveConversionB TypeByte t = if elem t [TypeByte, TypeShort, TypeInt, TypeChar] then [t] else []
primitiveConversionB TypeShort t = if elem t [TypeByte, TypeShort, TypeInt, TypeChar] then [t] else []
primitiveConversionB TypeInt t = if elem t [TypeByte, TypeShort, TypeInt, TypeChar] then [t] else []
primitiveConversionB TypeChar t = if elem t [TypeByte, TypeShort, TypeInt, TypeChar] then [t] else []
primitiveConversionB _ _ = []

-- for assignment
objectConversionA :: TypeNode -> Type -> Type -> [Type]
objectConversionA _ (Array _) (Object (Name ["java", "lang", "Object"])) = [(Object (Name ["java", "lang", "Object"]))]
objectConversionA _ (Array _) (Object (Name ["java", "lang", "Cloneable"])) = [(Object (Name ["java", "lang", "Cloneable"]))]
objectConversionA _ (Array _) (Object (Name ["java", "io", "Serializable"])) = [(Object (Name ["java", "io", "Serializable"]))]
objectConversionA typeDB (Array x@(Object _)) (Array y@(Object _)) = if null $ objectConversionA typeDB x y then [] else [Array y]
objectConversionA _ (Array x) (Array y) = if isPrimitive x && isPrimitive y && x == y then [Array y] else []
objectConversionA _ (Object _) (Object (Name ["java", "lang", "Object"])) = [(Object (Name ["java", "lang", "Object"]))]
objectConversionA typeDB (Object (Name x)) (Object (Name y))
    | x == y = [Object (Name x)]
    | otherwise = if isJust msymbolH && Just symbolY == msymbolH  then [(Object (Name y))] else []
    where
        symbolX = case getTypeEntry typeDB x of
                        Nothing -> error ("XXX" ++ show x)
                        _ -> symbol . fromJust $ getTypeEntry typeDB x
        symbolY = case getTypeEntry typeDB y of
                        Nothing -> error ("YYY" ++ show y)
                        _ -> symbol . fromJust $ getTypeEntry typeDB y
        msymbolH = higherInChain symbolX symbolY typeDB
objectConversionA _ _ _ = []

-- for casting
objectConversionB :: TypeNode -> Type -> Type -> [Type]
objectConversionB _ (Object (Name ["java", "lang", "Object"])) (Array x) = [(Array x)]
objectConversionB _ (Array _) (Object (Name ["java", "lang", "Object"])) = [(Object (Name ["java", "lang", "Object"]))]
objectConversionB _ (Array _) (Object (Name ["java", "lang", "Cloneable"])) = [(Object (Name ["java", "lang", "Cloneable"]))]
objectConversionB _ (Array _) (Object (Name ["java", "io", "Serializable"])) = [(Object (Name ["java", "io", "Serializable"]))]
objectConversionB typeDB (Array x@(Object _)) (Array y@(Object _)) = if null $ objectConversionB typeDB x y then [] else [Array y]
objectConversionB _ (Array x) (Array y) = if isPrimitive x && isPrimitive y && x == y then [Array y] else []
objectConversionB _ (Object _) (Object (Name ["java", "lang", "Object"])) = [(Object (Name ["java", "lang", "Object"]))]
objectConversionB typeDB (Object (Name x)) (Object (Name y))
    | x == y = [Object (Name x)]
    | otherwise = if casting then [(Object (Name y))] else []
    where
        symbolX = symbol . fromJust $ getTypeEntry typeDB x
        symbolY = symbol . fromJust $ getTypeEntry typeDB y
        casting = case higherInChain symbolX symbolY typeDB of
                    Nothing -> False --error $ (show x) ++ (show y) ++ (show symbolX) ++ (show symbolY)--False
                    Just x -> True

objectConversionB _ _ _ = []

----------------------------------------------------------

isPrimitive :: Type -> Bool
isPrimitive x = elem x [TypeBoolean, TypeChar, TypeByte, TypeShort, TypeInt]

{-
isObject :: Type -> Bool
isObject (Object _) = True
isObject (Array _) = True
isObject _ = False
-}

----------------------------------------------------------

boxingType :: Type -> Type
boxingType TypeBoolean = Object (Name ["java", "lang", "Boolean"])
boxingType TypeByte = Object (Name ["java", "lang", "Byte"])
boxingType TypeShort = Object (Name ["java", "lang", "Short"])
boxingType TypeInt = Object (Name ["java", "lang", "Integer"])
boxingType TypeChar = Object (Name ["java", "lang", "Character"])

unboxingType :: Type -> Maybe Type
unboxingType (Object (Name ["java", "lang", "Boolean"])) = Just TypeBoolean
unboxingType (Object (Name ["java", "lang", "Byte"])) = Just TypeByte
unboxingType (Object (Name ["java", "lang", "Short"])) = Just TypeShort
unboxingType (Object (Name ["java", "lang", "Integer"])) = Just TypeInt
unboxingType (Object (Name ["java", "lang", "Character"])) = Just TypeChar
unboxingType _ = Nothing


{-

isExtentedImplemented :: CompilationUnit -> TypeNode -> Type -> Type -> Bool
isExtentedImplemented unit typeDB (Object (Name x)) (Object (Name y)) = elem yNode xEI
	where
		unitImports = visibleImports unit
		xName = traverseTypeEntryWithImports typeDB unitImports x
		xNode = if xName == [] then error "ExtendedImplementedX" else fromJust $ getTypeEntry typeDB (head xName)
		yName = traverseTypeEntryWithImports typeDB unitImports y
		yNode = if yName == [] then error "ExtendedImplementedY" else fromJust $ getTypeEntry typeDB (head yName)
		xUnit = astUnit . symbol $ xNode
		xEI = getClassHierarchy xUnit typeDB
isExtentedImplemented _ _ _ _ = error "ExtendedImplemented: otherwise"

-}

-------------------------------------------------------------------------------
accessibleSymbol :: TypeNode -> [String] -> Symbol -> Bool
accessibleSymbol db cname sym = if init cname == init cnameTar
                                    then True
                                    else case higherInChain symbolFrom symbolTo db of
                                            Just symbolHigh -> if symbolHigh == symbolTo then True else False
                                            _ -> not $ elem "protected" mds
    where
        cnameTar = localScope sym
        mds = symbolModifiers sym
        symbolFrom = case getTypeEntry db cname of
                        Nothing -> error ("symbolFrom" ++ show cname)
                        Just s -> symbol s
        symbolTo = case getTypeEntry db cnameTar of
                        Nothing -> error ("symbolTo" ++ show cnameTar)
                        Just s -> symbol s

accessibleType :: TypeNode -> [String] -> Symbol -> Bool
accessibleType db cname sym = if init cname == init cnameTar
                                    then True
                                    else case higherInChain symbolFrom symbolTo db of
                                            Just symbolHigh -> if symbolHigh == symbolFrom then True else False
                                            _ -> not $ elem "protected" mds
    where
        symbolTo = sym
        cnameTar = (typeToName . localType) symbolTo
        mds = symbolModifiers sym
        symbolFrom = case getTypeEntry db cname of
                        Nothing -> error ("symbolFrom" ++ show cname)
                        Just s -> symbol s

traverseFieldEntryWithImports :: TypeNode -> [[String]] -> [String] -> [TypeNode]
traverseFieldEntryWithImports root imps query = nub . concat $ (flds ++ funcs)
    where
        cname = head imps
        entries = map (traverseTypeEntry root) imps
        entries' = map (\(mnode, imp) -> (fromJust mnode, imp)) (filter (isJust . fst) (zip entries imps))
        results = map (\(node, imp) -> (traverseTypeEntry node (init query), (init imp) ++ (init query))) ((root, ["*"]):entries')
        nm = if query == [] then error "traverseFieldEntryWithImports" else last query
        flds = [[tn | tn@(TN (SYM mds ls ln lt) ch) <- subNodes node, elem "static" mds, accessibleSymbol root cname (symbol tn), ln == nm] | (Just (TN _ [node]), cname) <- results]
        funcs = [[tn | tn@(TN (FUNC mds ls ln ps rt) ch) <- subNodes node, elem "static" mds, accessibleSymbol root cname (symbol tn), ln == nm] | (Just (TN _ [node]), cname) <- results]

traverseInstanceEntry' :: TypeNode -> TypeNode -> [String] -> [TypeNode]
traverseInstanceEntry' root tn@(TN (SYM _ _ _ _) _) [] = [tn]
traverseInstanceEntry' root tn@(TN (FUNC _ _ _ _ _) _) [] = [tn]
traverseInstanceEntry' root tn [] = []
traverseInstanceEntry' root (TN (SYM mds ls ln lt) _) (nm:cname) = traverseInstanceEntry' root root ((typeToName lt) ++ (nm:cname))
traverseInstanceEntry' root cur (nm:cname) = case [node | node <- subNodes cur, (localName . symbol) node == nm, (not $ isSYMFUNCNode node) || (not $ elem "static" ((symbolModifiers . symbol) node))] of
                                        []            -> []
                                        targets      -> concat $ map (\target -> traverseInstanceEntry' root target cname) targets

traverseInstanceEntryAccessible :: TypeNode -> [String] -> TypeNode -> [String] -> [TypeNode]
traverseInstanceEntryAccessible root cname tn@(TN (SYM _ _ _ _) _) [] = if accessibleSymbol root cname (symbol tn) then [tn] else []
traverseInstanceEntryAccessible root cname tn@(TN (FUNC _ _ _ _ _) _) [] = if accessibleSymbol root cname (symbol tn) then [tn] else []
traverseInstanceEntryAccessible root cname tn [] = []
traverseInstanceEntryAccessible root cname tn@(TN (SYM mds ls ln lt) _) (nm:remain) = if accessibleSymbol root cname (symbol tn) then traverseInstanceEntryAccessible root cname root ((typeToName lt) ++ (nm:remain)) else []
traverseInstanceEntryAccessible root cname cur (nm:remain) = case [node | node <- nodes, (localName . symbol) node == nm] of
                                                                []            -> []
                                                                targets      -> concat $ map (\target -> traverseInstanceEntryAccessible root cname target remain) targets
    where
        nodes = if isConcreteNode cur
                then [node | node <- subNodes cur, not $ elem "static" ((symbolModifiers . symbol) node), (accessibleType root cname (symbol cur)) || (not $ elem "protected" ((symbolModifiers . symbol) node))]
                else subNodes cur
