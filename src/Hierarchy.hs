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
  | any (\x -> "final" `elem` symbolModifiers x && (not $ "abstract" `elem` symbolModifiers x)) implementedMethods = Just $ "Class " ++ clsName ++ " overrides a final method"
  | (length clobberedMethods) > 0 = Just $ "Class " ++ clsName ++ " doesn't correctly implement an interface method: " ++ show clobberedMethods
  | (length interfaceConflicts) > 0 = Just $ "Class " ++ clsName ++ " implements conflicting interfaces which can't be satisfied: " ++ (show interfaceConflicts) ++ (show abstractMethods)
  | otherwise = Nothing
  where unitImports = visibleImports unit
        ownName = traverseTypeEntryWithImports typeDB unitImports [clsName]
        extendChain = getClassHierarchy unit typeDB
        implementedNames = map (traverseTypeEntryWithImports typeDB unitImports) implemented
        directImplementedNodes = if  any null implementedNames then error $ "no implementedNames for " ++ (show clsName) else mapMaybe (getTypeEntry typeDB) (map head implementedNames)
        directImplementedSymbols = map symbol directImplementedNodes
        implementedClasses = filter isClass directImplementedSymbols
        implementedNodes = concat $ map (\n -> (getClassInterfaces (astUnit . symbol $ n) typeDB)) extendChain
        implementedSymbols = map symbol implementedNodes
        abstractMethods = filter isFunction (map symbol (concat $ map subNodes implementedNodes))
        definedMethods = filter isFunction (map symbol (concat $ map subNodes extendChain))
        concreteDefinedMethods = filter (\x -> not $ "abstract" `elem` symbolModifiers x) definedMethods
        isAbstract = "abstract" `elem` modifiers
        (implementedMethods, unimplementedMethods) = partition (functionImplemented definedMethods) abstractMethods
        clobberedMethods = filter (functionClobbered concreteDefinedMethods) abstractMethods
        interfaceConflicts = filter (functionClobbered abstractMethods) abstractMethods
checkImplements _ _ = Nothing

checkExtends :: CompilationUnit -> TypeNode -> HierarchyError
checkExtends unit@(Comp _ _ (CLS clsMods clsName (Just extendee) _ _ _ _ _) _) typeDB
  | isNothing extendedNodeMaybe = Just $ "Class " ++ clsName ++ " tried to extend non-existent class " ++ (show extendee)
  | isInterface extendedClass = Just $ "Class " ++ clsName ++ " cannot extend interface " ++ (interfaceName extendedClass)
  | "final" `elem` (modifiers extendedClass) = Just $ "Class " ++ clsName ++ " cannot extend final class " ++ (className extendedClass)
  | extendedUnit == unit = Just $ "Class " ++ clsName ++ " cannot extend itself"
  | null hierarchyChain = Just $ "Class " ++ clsName ++ " has a circular extend reference"
  | any (\x -> "final" `elem` symbolModifiers x) overridenMethods = Just $ "Class " ++ clsName ++ " redeclared a final method"
  | any (\x -> "static" `elem` symbolModifiers x) overridenMethods = Just $ "Class " ++ clsName ++ " redeclared a static method"
  | (not isAbstract) && (any (\x -> "abstract" `elem` symbolModifiers x) nonoverridenMethods) = Just $ "Class " ++ clsName ++ " doesn't define all abstract methods"
  | (length clobberedMethods) > 0 = Just $ "Class " ++ clsName ++ " overwrites a final or static method"
  | (length hierarchyNoDefault) > 0 = Just $ "Class " ++ (show $ head hierarchyNoDefault) ++ " has no default constructor"
  | otherwise = Nothing
  where extendedNodeMaybe = getClassSuper unit typeDB
        extendedUnit = astUnit . symbol . fromJust $ extendedNodeMaybe
        extendedClass = definition extendedUnit
        hierarchyChain = getClassHierarchy unit typeDB
        hierarchyNoDefault = filter (not . hasDefaultConstructor) (tail hierarchyChain)
        ownNode = if hierarchyChain == [] then error "checkExtends" else head hierarchyChain
        extendeeMethods = filter isFunction (map symbol (concat $ map subNodes (tail hierarchyChain)))
        ownMethods = filter isFunction (map symbol (subNodes ownNode))
        isAbstract = "abstract" `elem` clsMods
        (overridenMethods, nonoverridenMethods) = partition (functionImplemented ownMethods) extendeeMethods
        clobberedMethods = filter (functionClobbered ownMethods) extendeeMethods

checkExtends unit@(Comp _ _ (ITF _ itfName extended _ _) _) typeDB
  | not . null $ extendedClasses = Just $ "Interface " ++ itfName ++ " cannot extend class " ++ (show $ head extendedClasses)
  | nub extendedClasses /= extendedClasses = Just $ "Interface " ++ itfName ++ " extends the same interface twice"
  | ownName `elem` extendedNames = Just $ "Interface " ++ itfName ++ " cannot extend itself"
  | null hierarchyChain = Just $ "Interface " ++ itfName ++ " extends a circular extend chain"
  | (length clobberedMethods) > 0 = Just $ "Interface " ++ itfName ++ " incorrectly overwrites an extended interface"
  | otherwise = Nothing
  where unitImports = visibleImports unit
        ownName = traverseTypeEntryWithImports typeDB unitImports [itfName]
        ownNode = if ownName == [] then error "checkImplements" else fromJust $ getTypeEntry typeDB (head ownName)
        extendedNames = map (traverseTypeEntryWithImports typeDB unitImports) extended
        extendedNodes = if any null extendedNames then error $ "checkExtends: no extendedNames " ++ (show itfName) ++ (show extended) ++ (show extendedNames) else mapMaybe (getTypeEntry typeDB) (map head extendedNames)
        extendedSymbols = map symbol extendedNodes
        extendedClasses = filter isClass extendedSymbols
        hierarchyChain = getInterfaceSupers unit typeDB
        ownMethods = filter isFunction (map symbol (subNodes ownNode))
        extendeeMethods = filter isFunction (map symbol (concat $ map subNodes (tail hierarchyChain)))
        clobberedMethods = filter (functionClobberedInterface ownMethods) extendeeMethods
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

getInterfaceSupers :: CompilationUnit -> TypeNode -> [TypeNode]
getInterfaceSupers unit@(Comp _ _ (ITF _ itfName _ _ _) _) typeDB =
  let unitImports = visibleImports unit
      ownName = traverseTypeEntryWithImports typeDB unitImports [itfName]
      ownNode = if ownName == [] then error "getClassHierarchy" else fromJust $ getTypeEntry typeDB (head ownName)
  in getInterfaceSupers' ownNode typeDB []

getInterfaceSupers' :: TypeNode -> TypeNode -> [TypeNode] -> [TypeNode]
getInterfaceSupers' node@(TN sym@(IT _ _ _ unit@(Comp _ _ (ITF _ itfName extended _ _) _)) _) typeDB hierarchy
  | null extended = [node]
  | node `elem` hierarchy = []
  | any null extendedInterfaceExtensions = []
  | otherwise = nub (node : (concat extendedInterfaceExtensions))
  where unitImports = visibleImports unit
        extendedNames = map (traverseTypeEntryWithImports typeDB unitImports) extended
        extendedNodes = if any null extendedNames then error $ "no extendedNames " ++ (show itfName) ++ (show extendedNames) else mapMaybe (getTypeEntry typeDB) (map head extendedNames)
        newHierarchy = node : hierarchy
        extendedInterfaceExtensions = map (\x -> getInterfaceSupers' x typeDB newHierarchy) extendedNodes
getInterfaceSupers' _ _ _ = []

getClassHierarchy :: CompilationUnit -> TypeNode -> [TypeNode]
getClassHierarchy unit@(Comp _ _ cls@(CLS _ clsName _ _ _ _ _ _) _) typeDB =
  let unitImports = visibleImports unit
      ownName = traverseTypeEntryWithImports typeDB unitImports [clsName]
      ownNode = if ownName == [] then error $ "getClassHierarchy error with imports " ++ (show unitImports) ++ (show unit) else fromJust $ getTypeEntry typeDB (head ownName)
  in getClassHierarchyForSymbol ownNode typeDB

{- this function should not be called
getClassHierarchy unit@(Comp _ _ (ITF _ itfName _ _ _ ) _) typeDB =
  let unitImports = visibleImports unit
      ownName = traverseTypeEntryWithImports typeDB unitImports [itfName]
      ownNode = if ownName == [] then error "getClassHierarchy" else fromJust $ getTypeEntry typeDB (head ownName)
  in getClassHierarchyForSymbol ownNode typeDB
-}

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

getClassInterfaces :: CompilationUnit -> TypeNode -> [TypeNode]
getClassInterfaces unit@(Comp _ _ (CLS modifiers clsName _ implemented _ _ _ _) _) typeDB =
  let unitImports = visibleImports unit
      ownName = traverseTypeEntryWithImports typeDB unitImports [clsName]
      ownNode = if ownName == [] then error "checkImplements" else fromJust $ getTypeEntry typeDB (head ownName)
      implementedNames = map (traverseTypeEntryWithImports typeDB unitImports) implemented
      implementedNodes = if any null implementedNames then [] {- error $ "no implementedNames for " ++ (show clsName)-} else mapMaybe (getTypeEntry typeDB) (map head implementedNames)
      hierarchyChain = nub . concat $ map (\x -> getInterfaceSupers' x typeDB []) implementedNodes
  in hierarchyChain
getClassInterfaces _ _ = []

functionClobbered :: [Symbol] -> Symbol -> Bool
functionClobbered definitions fun =
  -- A is parent, b is child (replacement)
  let funEqual a b = (localName a == localName b && parameterTypes a == parameterTypes b && localName a /= (last . typeToName . localType $ a))  &&
                       (("static" `elem` symbolModifiers b &&
                         not ("static" `elem` (symbolModifiers a))) ||
                        ((localType a) /= (localType b)) ||
                        ("public" `elem` symbolModifiers a && "protected" `elem` symbolModifiers b && localScope b /= ["java", "lang", "ObjectInterface"] && localScope b /= ["java", "lang", "Object"]))
  in any (funEqual fun) definitions

functionClobberedInterface :: [Symbol] -> Symbol -> Bool
functionClobberedInterface definitions fun =
  -- A is parent, b is child (replacement)
  let funEqual a b = (localName a == localName b && localName a /= (last . typeToName . localType $ a))  &&
                       (("static" `elem` symbolModifiers b &&
                         not ("static" `elem` (symbolModifiers a)) &&
                         parameterTypes a == parameterTypes b) ||
                        (localType a) /= (localType b) ||
                        ("final" `elem` symbolModifiers a) ||
                        ("public" `elem` symbolModifiers a && "protected" `elem` symbolModifiers b &&
                          parameterTypes a == parameterTypes b))
  in any (funEqual fun) definitions

functionImplemented :: [Symbol] -> Symbol -> Bool
functionImplemented definitions fun =
  let funEqual a b = (localName a == localName b) &&
                     (parameterTypes a == parameterTypes b) &&
                     (localType a == localType b) &&
                     (not $ "abstract" `elem` (symbolModifiers b))
  in any (funEqual fun) definitions

-- Class -> Class -> Type Database -> highest || Nothing if symbols are unrelated
higherInChain :: Symbol -> Symbol -> TypeNode -> Maybe Symbol
higherInChain symA@(CL _ _ _ unitA) symB@(CL _ _ _ unitB) typeDB
  | symA `elem` hierarchyB = Just symA
  | symB `elem` hierarchyA = Just symB
  | otherwise = Nothing
  where hierarchyA = map symbol $ getClassHierarchy unitA typeDB
        hierarchyB = map symbol $ getClassHierarchy unitB typeDB

higherInChain symA@(IT _ _ _ unitA) symB@(IT _ _ _ unitB) typeDB
  | symA `elem` hierarchyB = Just symA
  | symB `elem` hierarchyA = Just symB
  | otherwise = Nothing
  where hierarchyA = map symbol $ getInterfaceSupers unitA typeDB
        hierarchyB = map symbol $ getInterfaceSupers unitB typeDB

higherInChain symA@(CL _ _ _ unitA) symB@(IT _ _ _ _) typeDB
  | symB `elem` hierarchyInterface = Just symB
  | otherwise = Nothing
  where hierarchyClass = map symbol $ getClassHierarchy unitA typeDB
        hierarchyInterface = map symbol $ concat $ map (\x -> getClassInterfaces (astUnit x) typeDB) hierarchyClass

higherInChain _ _ _ = Nothing--error "higherInChain: casting an interface to a class"

isA :: TypeNode -> Symbol -> Symbol -> Bool
isA db symA symB = case parent of
                     Just sym -> sym == symB
                     _ -> False
  where
    parent = higherInChain symA symB db

hasDefaultConstructor :: TypeNode -> Bool
hasDefaultConstructor node@(TN sym@(CL _ _ _ unit@(Comp _ _ (CLS _ _ _ _ constructors _ _ _) _)) _) =
  any (\c -> (length . constructorParameters $ c) == 0) constructors
