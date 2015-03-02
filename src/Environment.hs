module Environment where

import Control.Monad
import Data.List
import Data.Maybe

import AST
import Util

data Kind = Package | Class | Interface | Method | Field | Statement | Var Expression | Exp Expression | Ret Expression | WhileBlock Expression | IfBlock Expression | ForBlock deriving (Eq, Show)

data Symbol = SYM {
    symbolModifiers :: [String],
    localName :: String,
    localType :: Type
}           | PKG {
    localName :: String
}           | CL {
    symbolModifiers :: [String],
    localName :: String
}           | IT {
    symbolModifiers :: [String],
    localName :: String
}           | FUNC {
    symbolModifiers :: [String],
    localName :: String,
    parameterTypes :: [Type],
    localType :: Type
} deriving (Eq, Show)

data SemanticUnit = Root {
    scope :: [String]
}                   | SU {
    scope :: [String],
    kind :: Kind,
    symbolTable :: [Symbol],
    inheritFrom :: SemanticUnit
} deriving (Eq)

instance Show SemanticUnit where
  show (SU scope kind table from) = (show kind) ++ ": " ++ (show scope) ++ "\n"
  show (Root scope) = "ROOT: " ++ (show scope) ++ "\n"


buildSymbolFromConstructor (Cons constructorModifiers constructorName constructorParameters constructorInvocation constructorDefinition consi) = FUNC constructorModifiers constructorName (map typeName constructorParameters) (Object (Simple constructorName))
buildSymbolFromField (FLD fieldModifiers (TV tp nm ai) fieldValue fldi) = SYM fieldModifiers nm tp
buildSymbolFromMethod (MTD methodModifiers (TV tp nm ai) methodParameters methodDefinition mtdi) = FUNC methodModifiers nm (map typeName methodParameters) tp
buildSymbolFromParameter (TV tp nm ai) = SYM [] nm tp

data Environment = ENVE | ENV {
    semantic :: SemanticUnit,
    children :: [Environment]
} deriving (Eq)

instance Show Environment where
  show (ENVE) = "[Empty]\n"
  show (ENV unit kids) = "{\n" ++
                         "  " ++ show (unit) ++ "\n" ++
                         (indent 2 (intercalate "\n" lns)) ++ "\n" ++
                         "}\n"
    where lns = map show kids

buildEnvironment :: CompilationUnit -> Environment
buildEnvironment (Comp pkg imps def cui) = buildEnvironmentWithPackage cname (Root []) def
    where
        cname = if isNothing pkg then [] else fromJust pkg

buildEnvironmentWithPackage [] parent def = ENV su env
    where
        cname' = [[]]
        su = case def of
                (CLS mds nm ext imps cons flds mtds clsi)   -> SU cname' Package [CL mds nm] parent
                (ITF mds nm imps mtds itfi)                 -> SU cname' Package [IT mds nm] parent
        env = case def of
                (CLS mds nm ext imps cons flds mtds clsi)   -> [buildEnvironmentFromClass su (CLS mds nm ext imps cons flds mtds clsi)]
                (ITF mds nm imps mtds itfi)                 -> [buildEnvironmentFromInterface su (ITF mds nm imps mtds itfi)]

buildEnvironmentWithPackage [nm] parent def = ENV su env
    where
        cname' = ((scope parent) ++ [nm])
        su = case def of
                (CLS mds nm ext imps cons flds mtds clsi)   -> SU cname' Package [CL mds nm] parent
                (ITF mds nm imps mtds itfi)                 -> SU cname' Package [IT mds nm] parent
        env = case def of
                (CLS mds nm ext imps cons flds mtds clsi)   -> [buildEnvironmentFromClass su (CLS mds nm ext imps cons flds mtds clsi)]
                (ITF mds nm imps mtds itfi)                 -> [buildEnvironmentFromInterface su (ITF mds nm imps mtds itfi)]

buildEnvironmentWithPackage cname parent def = ENV su env
    where
        nm = last cname
        cname' = ((scope parent) ++ [nm])
        su = SU cname' Package [] parent
        env = [buildEnvironmentWithPackage (init cname) su def]

buildEnvironmentFromClass parent (CLS mds nm ext imps cons flds mtds clsi) = env
    where
        cname' = ((scope parent) ++ [nm])
        syms = (map buildSymbolFromConstructor cons) ++ (map buildSymbolFromField flds) ++ (map buildSymbolFromMethod mtds)
        su = (SU cname' Class syms parent)
        flds' = map (buildEnvironmentFromField su) flds
        mtds' = map (buildEnvironmentFromMethod su) mtds
        env = ENV su (flds' ++ mtds')
buildEnvironmentFromInterface parent (ITF mds nm imps mtds itfi) = env
    where
        cname' = ((scope parent) ++ [nm])
        syms = (map buildSymbolFromMethod mtds)
        su = (SU cname' Interface syms parent)
        mtds' = map (buildEnvironmentFromMethod su) mtds
        env = ENV su (mtds')

buildEnvironmentFromField :: SemanticUnit -> Field -> Environment
buildEnvironmentFromField parent (FLD fieldModifiers fieldVar fieldValue fldi) = ENV (SU cname' Field [] parent) []
    where
        cname' = ((scope parent) ++ [varName fieldVar])

buildEnvironmentFromMethod :: SemanticUnit -> Method -> Environment
buildEnvironmentFromMethod parent (MTD methodModifiers methodVar methodParameters methodDefinition mtdi) = ENV su ch
    where
        cname' = ((scope parent) ++ [varName methodVar])
        syms = (map buildSymbolFromParameter methodParameters)
        su = (SU cname' Method syms parent)
        ch = if isNothing methodDefinition then [] else [buildEnvironmentFromStatements su (statements (fromJust methodDefinition))]

buildEnvironmentFromStatements :: SemanticUnit -> [Statement] -> Environment
buildEnvironmentFromStatements parent [] = ENVE
buildEnvironmentFromStatements parent ((Empty):remain) = buildEnvironmentFromStatements parent remain
buildEnvironmentFromStatements parent ((LocalVar var val):remain) = ENV su [buildEnvironmentFromStatements su remain]
    where
        cname' = (scope parent)
        su = (SU cname' (Var val) [buildSymbolFromParameter var] parent)
buildEnvironmentFromStatements parent ((Expr expr):remain) = ENV su [buildEnvironmentFromStatements parent remain]
    where
        cname' = (scope parent)
        su = (SU cname' (Exp expr) [] parent)
buildEnvironmentFromStatements parent ((Return mexpr):remain) = if isNothing mexpr then buildEnvironmentFromStatements parent remain else ENV su [buildEnvironmentFromStatements parent remain]
    where
        cname' = (scope parent)
        expr = fromJust mexpr
        su = (SU cname' (Ret expr) [] parent)
buildEnvironmentFromStatements parent ((Block sb):remain) = ENV su [buildEnvironmentFromStatements su (statements sb), buildEnvironmentFromStatements parent remain]
    where
        cname' = ((scope parent) ++ [[]])
        su = (SU cname' Statement [] parent)

buildEnvironmentFromStatements parent ((While expr sb):remain) = ENV su [buildEnvironmentFromStatements su (statements sb), buildEnvironmentFromStatements parent remain]
    where
        cname' = ((scope parent) ++ [[]])
        su = (SU cname' (WhileBlock expr) [] parent)

buildEnvironmentFromStatements parent ((For mistmt mexpr msstmt sb):remain) = ENV su [e0, e1, e2, buildEnvironmentFromStatements su (statements sb), buildEnvironmentFromStatements parent remain]
    where
        cname' = ((scope parent) ++ [[]])
        syms = case mistmt of
            Just (LocalVar var val) -> [buildSymbolFromParameter var]
            _ -> []
        su = (SU cname' ForBlock syms parent)
        bld = buildEnvironmentFromStatements su
        e0 = case mistmt of
            Just (LocalVar var val) -> bld ((Expr val):[])
            Just a -> bld (a:[])
            _ -> ENVE
        e1 = case mexpr of
            Just a -> bld ((Expr a):[])
            _ -> ENVE
        e2 = case msstmt of
            Just a -> bld (a:[])
            _ -> ENVE


buildEnvironmentFromStatements parent ((If expr isb mesb):remain) = ENV su [e0, e1, buildEnvironmentFromStatements parent remain]
    where
        cname' = ((scope parent) ++ [[]])
        su = (SU cname' (IfBlock expr) [] parent)
        bld = buildEnvironmentFromStatements su
        e0 = bld (statements isb)
        e1 = case mesb of
            Just tsb -> bld (statements tsb)
            _ -> ENVE

getLocalEnvironment :: CompilationUnit -> [Environment] -> Environment
getLocalEnvironment unit envs
  | length directEnvironments /= length (nub directNames) = ENVE
  | any (\group -> length group == 0) wildCardEnvironments = ENVE
  | otherwise = ENV (Root []) $ nub $ aliasedEnvironments ++ envs
  where packageName = case package unit of { Just pkgName -> pkgName; _ -> [unitName $ definition unit] }
        packageEnvironments = filter (environmentIsInPackage packageName) envs
        javaEnvironments = filter (environmentIsInPackage ["java", "lang"]) envs
        (wildcardNames, directNames) = partition (elem "*") (imports unit)
        directEnvironments = filter (environmentHasScope directNames) envs
        wildcardNamesNoStar = map init wildcardNames
        wildCardEnvironments = map (\name -> filter (environmentIsInPackage name) envs) wildcardNamesNoStar
        accessibleEnvironments = packageEnvironments ++ directEnvironments ++ (concat wildCardEnvironments) ++ javaEnvironments
        aliasedEnvironments = map getImportedEnvironment accessibleEnvironments

getImportedEnvironment :: Environment -> Environment
getImportedEnvironment (ENV (SU scope kind st parent) children) = (ENV (SU [(last scope)] kind st parent) children)
getImportedEnvironment env = env

environmentHasScope :: [[String]] -> Environment -> Bool
environmentHasScope pkgNames (ENV (SU scope _ _ _) _) = scope `elem` pkgNames
environmentHasScope _ _ = False

environmentIsInPackage :: [String] -> Environment -> Bool
environmentIsInPackage pkgName (ENV (SU scope _ _ _) _)
  | take pkgLength scope == pkgName = True
  | otherwise = False
  where pkgLength = length pkgName
environmentIsInPackage _ _ = False

findUnitInEnv :: [String] -> Kind -> Environment -> Maybe SemanticUnit
findUnitInEnv _ _ ENVE = Nothing
findUnitInEnv name searchKind (ENV (Root _) children) =
  msum $ map (findUnitInEnv name searchKind) children
findUnitInEnv name searchKind (ENV unit children)
  | last (scope unit) == last name && (kind unit) == searchKind = Just unit
  | otherwise = msum $ map (findUnitInEnv name searchKind) children

getClassSymbol :: SemanticUnit -> String -> Maybe Symbol
getClassSymbol (Root _)_ = Nothing
getClassSymbol (SU _ _ table parent) name =
  let isClass targetName (CL _ name) = targetName == name
      isClass _ _ = False
  in
    case find (isClass name) table of
      Just symbol -> Just symbol
      Nothing -> getClassSymbol parent name

getInterfaceSymbol :: SemanticUnit -> String -> Maybe Symbol
getInterfaceSymbol (Root _) _ = Nothing
getInterfaceSymbol (SU _ _ table parent) name =
  let isInterface targetName (IT _ name) = targetName == name
      isInterface _ _ = False
  in
    case find (isInterface name) table of
      Just symbol -> Just symbol
      Nothing -> getInterfaceSymbol parent name
