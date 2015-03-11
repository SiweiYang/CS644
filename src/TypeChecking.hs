module TypeChecking where

import AST
import Environment
import TypeDatabase
import Hierarchy
import Data.Maybe

conversion :: TypeNode -> Type -> Type -> [Type]
conversion typeDB typeS typeT
    | typeS == typeT = [typeT]
    | typeT == TypeNull = []
    | typeS == TypeNull = case typeT of
                            (Object x) -> [(Object x)]
                            TypeString -> [TypeString]
                            _ -> []
    | otherwise = case (isPrimitive typeS, isPrimitive typeT) of
                            (True, True) ->  typeS:(primitiveConversion typeS typeT)
                            (False, True) -> case (unboxed == typeT) of
                                                True -> [typeS, typeT]
                                                False -> if null nextUnbox then [] else [typeS, unboxed] ++ nextUnbox
                            (False, False) -> typeS:(objectConversion typeDB typeS typeT)
                            (True, False) -> case (boxed == typeT) of
                                                True -> [typeS, typeT]
                                                False -> if null nextBox then [] else [typeS, boxed] ++ nextBox
    where
        boxed = boxingType typeS
        nextBox = objectConversion typeDB boxed typeT
        unboxed = unboxingType typeS
        nextUnbox = primitiveConversion unboxed typeT


----------------------------------------------------------

primitiveConversion :: Type -> Type -> [Type]
primitiveConversion TypeBoolean _ = []
primitiveConversion TypeByte TypeShort = [TypeShort]
primitiveConversion TypeByte TypeInt = [TypeInt]
primitiveConversion TypeShort TypeInt = [TypeInt]
primitiveConversion TypeChar TypeInt = [TypeInt]
primitiveConversion _ _ = []

objectConversion :: TypeNode -> Type -> Type -> [Type]
objectConversion _ (Array _) (Object (Name ["java", "lang", "Object"])) = [(Object (Name ["java", "lang", "Object"]))]
objectConversion _ (Array _) (Object (Name ["java", "lang", "Cloneable"])) = [(Object (Name ["java", "lang", "Cloneable"]))]
objectConversion _ (Array _) (Object (Name ["java", "io", "Serializable"])) = [(Object (Name ["java", "io", "Serializable"]))]
objectConversion typeDB (Array x@(Object _)) (Array y@(Object _)) = if null $ objectConversion typeDB x y then [] else [Array y]
objectConversion _ (Array x) (Array y) = if isPrimitive x && isPrimitive y && x == y then [Array y] else []
objectConversion _ (Object _) (Object (Name ["java", "lang", "Object"])) = [(Object (Name ["java", "lang", "Object"]))]
objectConversion typeDB (Object (Name x)) (Object (Name y))
    | x == y = [Object (Name x)]
    | otherwise = if isJust $ higherInChain symbolX symbolY typeDB then [(Object (Name y))] else []
    where
        symbolX = symbol . fromJust $ getTypeEntry typeDB x
        symbolY = symbol . fromJust $ getTypeEntry typeDB y
objectConversion _ _ _ = []

----------------------------------------------------------

isPrimitive :: Type -> Bool
isPrimitive x = elem x [TypeBoolean, TypeChar, TypeString, TypeByte, TypeShort, TypeInt]

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
boxingType TypeString = Object (Name ["java", "lang", "String"])

unboxingType :: Type -> Type
unboxingType (Object (Name ["java", "lang", "Boolean"])) = TypeBoolean
unboxingType (Object (Name ["java", "lang", "Byte"])) = TypeByte
unboxingType (Object (Name ["java", "lang", "Short"])) = TypeShort
unboxingType (Object (Name ["java", "lang", "Integer"])) = TypeInt
unboxingType (Object (Name ["java", "lang", "Character"])) = TypeChar
unboxingType (Object (Name ["java", "lang", "String"])) = TypeString

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


