module TypeChecking where

import AST
import Environment
import TypeDatabase
import TypeLinking
import Hierarchy
import Data.Maybe

assignConversion :: CompilationUnit -> TypeNode -> Type -> Type -> Bool

assignConversion _ _ _ (Object (Name ["String"])) = True
assignConversion _ _ TypeVoid t = t == TypeVoid
assignConversion _ _ TypeNull (Object _) = True
assignConversion _ _ TypeNull (Array _) = True
--assignConversion TypeNull TypeNull = True
assignConversion _ _ TypeByte t = elem t [TypeByte, TypeShort, TypeInt]
assignConversion _ _ TypeShort t = elem t [TypeShort, TypeInt]
assignConversion _ _ TypeInt t = t == TypeInt
assignConversion _ _ TypeBoolean t = t == TypeBoolean
assignConversion _ _ TypeChar t = elem t [TypeChar, TypeInt]
assignConversion _ _ TypeString t = t == TypeString
assignConversion _ _ (Array _) (Object (Name ["Object"])) = True
assignConversion _ _ (Array _) (Object (Name ["Cloneable"])) = True
assignConversion _ _ (Array _) (Object (Name ["java", "io", "Serializable"])) = True
assignConversion unit typeDB (Array x@(Object _)) (Array y@(Object _)) = assignConversion unit typeDB x y
assignConversion _ _ (Array x) (Array y) = x == y && elem x [TypeByte, TypeShort, TypeInt, TypeBoolean, TypeChar, TypeString]
assignConversion _ _ (Object _) (Object (Name ["Object"])) = True
assignConversion unit typeDB x@(Object _) y@(Object _)
	| x == y = True
 	| otherwise = isExtentedImplemented unit typeDB x y
assignConversion _ _ _ _ = False

castConversion :: CompilationUnit -> TypeNode -> Type -> Type -> Bool
castConversion _ _ TypeVoid t = False
castConversion _ _ TypeBoolean t = t == TypeBoolean
castConversion _ _ TypeNull t = t == TypeNull
castConversion _ _ TypeByte t = elem t [TypeByte, TypeShort, TypeInt]
castConversion _ _ TypeShort t = elem t [TypeShort, TypeInt]
castConversion _ _ TypeInt t = t == TypeInt
castConversion _ _ TypeChar t = elem t [TypeChar, TypeInt]
castConversion _ _ TypeString t = t == TypeString
castConversion unit typeDB x y = assignConversion unit typeDB x y


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



