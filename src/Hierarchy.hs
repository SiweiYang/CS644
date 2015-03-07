module Hierarchy where

import Control.Monad
import Data.Maybe
import Data.List

import AST
import Environment
import TypeDatabase
import Util

type HierarchyError = Maybe String

checkHierarchies :: [CompilationUnit] -> TypeNode -> HierarchyError
checkHierarchies units typeDB = msum $ map (\unit -> checkHierarchy unit typeDB) units

checkHierarchy :: CompilationUnit -> TypeNode -> HierarchyError
checkHierarchy unit typeDB
  | isJust implementError = implementError
  | isJust extendError = extendError
  | isJust importError = importError
  | otherwise = Nothing
  where implementError = checkImplements unit typeDB
        extendError = checkExtends unit typeDB
        importError = checkImports unit typeDB

checkImports :: CompilationUnit -> TypeNode -> HierarchyError
checkImports unit@(Comp _ imports def _) typeDB
  | any isNothing importedPackages = Just ("Invalid import - package not found" ++ (show imports) ++ (show importedPackages))
  | visibleNames /= nub visibleNames = Just "Invalid import - name conflict"
  | otherwise = Nothing
  where unitImports = nub $ visibleImports unit
        importedPackages = map (traverseNodeEntry typeDB) imports
        visibleNames = [last cname | cname <- unitImports, last cname /= "*"]

checkImplements :: CompilationUnit -> TypeNode -> HierarchyError
checkImplements unit@(Comp _ _ (CLS modifiers clsName _ implemented _ _ _ _) _) typeDB
  | any null implementedNames = Just $ "Class " ++ clsName ++ " extends a non-existent interface"
  | not . null $ implementedClasses = Just $ "Class " ++ clsName ++ " cannot implement class " ++ (localName $ head implementedClasses)
  | nub implementedNames /= implementedNames = Just $ "Class " ++ clsName ++ " implements the same interface twice"
  | (not isAbstract) && (not . null $ unimplementedMethods) = Just $ "Class " ++ clsName ++ " doesn't implement methods " ++ (show $ map localName unimplementedMethods)
  | any (\x -> "final" `elem` symbolModifiers x) implementedMethods = Just $ "Class " ++ clsName ++ " overrides a final method"
  | otherwise = Nothing
  where unitImports = visibleImports unit
        ownName = traverseTypeEntryWithImports typeDB unitImports [clsName]
        ownNode = if ownName == [] then error "checkImplements" else fromJust $ getTypeEntry typeDB (head ownName)
        implementedNames = map (traverseTypeEntryWithImports typeDB unitImports) implemented
        implementedNodes = mapMaybe (getTypeEntry typeDB) (map head implementedNames)
        implementedSymbols = map symbol implementedNodes
        implementedClasses = filter isClass implementedSymbols
        extendChain = getClassHierarchy unit typeDB
        abstractMethods = filter isFunction (map symbol (concat $ map subNodes implementedNodes))
        definedMethods = filter isFunction (map symbol (concat $ map subNodes extendChain))
        isAbstract = "abstract" `elem` modifiers
        (implementedMethods, unimplementedMethods) = partition (functionImplemented definedMethods) abstractMethods
checkImplements _ _ = Nothing

checkExtends :: CompilationUnit -> TypeNode -> HierarchyError
checkExtends unit@(Comp _ _ (CLS clsMods clsName (Just extendee) _ _ _ _ _) _) typeDB
  | isNothing extendedNodeMaybe = Just $ "Class " ++ clsName ++ " tried to extend non-existent class " ++ (show extendee)
  | isInterface extendedClass = Just $ "Class " ++ clsName ++ " cannot extend interface " ++ (interfaceName extendedClass)
  | "final" `elem` (modifiers extendedClass) = Just $ "Class " ++ clsName ++ " cannot extend final class " ++ (className extendedClass)
  | extendedUnit == unit = Just $ "Class " ++ clsName ++ " cannot extend itself"
  | null hierarchyChain = Just $ "Class " ++ clsName ++ " has a circular extend reference"
  | any (\x -> "final" `elem` symbolModifiers x) overridenMethods = Just $ "Class " ++ clsName ++ " redeclared a final method"
  | (not isAbstract) && (any (\x -> "abstract" `elem` symbolModifiers x) nonoverridenMethods) = Just $ "Class " ++ clsName ++ " doesn't define all abstract methods"
  | otherwise = Nothing
  where extendedNodeMaybe = getClassSuper unit typeDB
        extendedUnit = astUnit . symbol . fromJust $ extendedNodeMaybe
        extendedClass = definition extendedUnit
        hierarchyChain = getClassHierarchy unit typeDB
        ownNode = if hierarchyChain == [] then error "checkExtends" else head hierarchyChain
        extendeeMethods = filter isFunction (map symbol (concat $ map subNodes (tail hierarchyChain)))
        ownMethods = filter isFunction (map symbol (subNodes ownNode))
        isAbstract = "abstract" `elem` clsMods
        (overridenMethods, nonoverridenMethods) = partition (functionImplemented ownMethods) extendeeMethods

checkExtends unit@(Comp _ _ (ITF _ itfName extended _ _) _) typeDB
  | not . null $ extendedClasses = Just $ "Interface " ++ itfName ++ " cannot extend class " ++ (show $ head extendedClasses)
  | nub extendedClasses /= extendedClasses = Just $ "Interface " ++ itfName ++ " extends the same interface twice"
  | ownName `elem` extendedNames = Just $ "Interface " ++ itfName ++ " cannot extend itself"
  | null hierarchyChain = Just $ "Interface " ++ itfName ++ " extends a circular extend chain"
  | otherwise = Nothing
  where unitImports = visibleImports unit
        ownName = traverseTypeEntryWithImports typeDB unitImports [itfName]
        extendedNames = map (traverseTypeEntryWithImports typeDB unitImports) extended
        extendedNodes = mapMaybe (getTypeEntry typeDB) (map head extendedNames)
        extendedSymbols = map symbol extendedNodes
        extendedClasses = filter isClass extendedSymbols
        hierarchyChain = getInterfaceSupers unit typeDB
checkExtends _ _ = Nothing

getClassSuper :: CompilationUnit -> TypeNode -> Maybe TypeNode
getClassSuper unit@(Comp _ _ (CLS _ clsName (Just extendee) _ _ _ _ _) _) typeDB
  | null extendedName = Nothing
  | not extendedNodeExists = Nothing
  | otherwise = extendedNode
  where classImports = visibleImports unit
        extendedName = traverseTypeEntryWithImports typeDB classImports extendee
        extendedNode = if extendedName == [] then error "getClassSuper" else getTypeEntry typeDB (head extendedName)
        extendedNodeExists = isJust extendedNode
getClassSuper _ _ = Nothing

getInterfaceSupers :: CompilationUnit -> TypeNode -> [CompilationUnit]
getInterfaceSupers unit typeDB = getInterfaceSupers' unit typeDB []

getInterfaceSupers' :: CompilationUnit -> TypeNode -> [CompilationUnit] -> [CompilationUnit]
getInterfaceSupers' unit@(Comp _ _ (ITF _ _ extended _ _) _) typeDB hierarchy
  | null extended = [unit]
  | unit `elem` hierarchy = []
  | any null extendedInterfaceExtensions = []
  | otherwise = nub (unit : (concat extendedInterfaceExtensions))
  where unitImports = visibleImports unit
        extendedNames = map (traverseTypeEntryWithImports typeDB unitImports) extended
        extendedNodes = mapMaybe (getTypeEntry typeDB) (map head extendedNames)
        extendedSymbols = map symbol extendedNodes
        extendedInterfaces = map astUnit extendedSymbols
        newHierarchy = unit : hierarchy
        extendedInterfaceExtensions = map (\x -> getInterfaceSupers' x typeDB newHierarchy) extendedInterfaces
getInterfaceSupers' _ _ _ = []

getClassHierarchy :: CompilationUnit -> TypeNode -> [TypeNode]
getClassHierarchy unit@(Comp _ _ (CLS _ clsName _ _ _ _ _ _) _) typeDB
  | otherwise = getClassHierarchyForSymbol ownNode typeDB
  where unitImports = visibleImports unit
        ownName = traverseTypeEntryWithImports typeDB unitImports [clsName]
        ownNode = if ownName == [] then error "getClassHierarchy" else fromJust $ getTypeEntry typeDB (head ownName)

getClassHierarchyForSymbol :: TypeNode -> TypeNode -> [TypeNode]
getClassHierarchyForSymbol node typeDB = getClassHierarchy' node typeDB [node]

getClassHierarchy' :: TypeNode -> TypeNode -> [TypeNode] -> [TypeNode]
getClassHierarchy' node@(TN sym@(CL _ _ _ unit@(Comp _ _ (CLS _ _ _ implemented _ _ _ _) _)) _) typeDB hierarchy
  | isNothing extendedClassMaybe = hierarchy
  | extendedClass `elem` hierarchy = []
  | otherwise = getClassHierarchy' extendedClass typeDB (hierarchy ++ [extendedClass])
  where extendedClassMaybe = getClassSuper unit typeDB
        extendedClass = fromJust extendedClassMaybe
getClassHierarchy' node@(TN sym@(IT _ _ _ unit@(Comp _ _ (ITF _ _ implemented _ _) _)) _) typeDB hierarchy
  | isNothing extendedClassMaybe = hierarchy
  | extendedClass `elem` hierarchy = []
  | otherwise = getClassHierarchy' extendedClass typeDB (hierarchy ++ [extendedClass])
  where extendedClassMaybe = getClassSuper unit typeDB
        extendedClass = fromJust extendedClassMaybe

functionImplemented :: [Symbol] -> Symbol -> Bool
functionImplemented definitions fun =
  let funEqual a b = (localName a == localName b) &&
                     (parameterTypes a == parameterTypes b) &&
                     (localType a == localType b) &&
                     (not $ "abstract" `elem` (symbolModifiers b))
  in any (funEqual fun) definitions
