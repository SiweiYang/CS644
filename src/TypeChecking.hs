module TypeChecking where

import AST
import Environment
import TypeDatabase
import TypeLinking
import Hierarchy
import Data.Maybe

assignConversion :: TypeNode -> Type -> Type -> Bool

assignConversion _ _ (Object (Name ["String"])) = True
assignConversion _ TypeVoid t = t == TypeVoid
assignConversion _ TypeNull (Object _) = True
assignConversion _ TypeNull (Array _) = True
--assignConversion TypeNull TypeNull = True
assignConversion _ TypeByte t = elem t [TypeByte, TypeShort, TypeInt]
assignConversion _ TypeShort t = elem t [TypeShort, TypeInt]
assignConversion _ TypeInt t = t == TypeInt
assignConversion _ TypeBoolean t = t == TypeBoolean
assignConversion _ TypeChar t = elem t [TypeChar, TypeInt]
assignConversion _ TypeString t = t == TypeString
assignConversion _ (Array _) (Object (Name ["Object"])) = True
assignConversion _ (Array _) (Object (Name ["Cloneable"])) = True
assignConversion _ (Array _) (Object (Name ["java", "io", "Serializable"])) = True
assignConversion typeDB (Array x@(Object _)) (Array y@(Object _)) = assignConversion typeDB x y
assignConversion _ (Array x) (Array y) = x == y && elem x [TypeByte, TypeShort, TypeInt, TypeBoolean, TypeChar, TypeString]
assignConversion _ (Object _) (Object (Name ["Object"])) = True
assignConversion unit typeDB (Object (Name x)) (Object (Name y))
	| x == y = True
 	| otherwise = 
 	where
 		symbolX = symbol . fromJust $ getTypeEntry typeDB x
 		symbolY = symbol . fromJust $ getTypeEntry typeDB y



assignConversion _ _ _ = False

castConversion :: TypeNode -> Type -> Type -> Bool
castConversion _ TypeVoid t = False
castConversion _ TypeBoolean t = t == TypeBoolean
castConversion _ TypeNull t = t == TypeNull
castConversion _ TypeByte t = elem t [TypeByte, TypeShort, TypeInt]
castConversion _ TypeShort t = elem t [TypeShort, TypeInt]
castConversion _ TypeInt t = t == TypeInt
castConversion _ TypeChar t = elem t [TypeChar, TypeInt]
castConversion _ TypeString t = t == TypeString
castConversion typeDB x y = assignConversion typeDB x y


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


