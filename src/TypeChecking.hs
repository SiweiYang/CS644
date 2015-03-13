module TypeChecking where

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


