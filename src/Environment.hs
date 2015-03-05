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
    localName :: String,
    localType :: Type,
    astUnit :: CompilationUnit
}           | IT {
    symbolModifiers :: [String],
    localName :: String,
    localType :: Type,
    astUnit :: CompilationUnit
}           | FUNC {
    symbolModifiers :: [String],
    localName :: String,
    parameterTypes :: [Type],
    localType :: Type
} deriving (Eq, Show)

isClass (CL _ _ _ _) = True
isClass _ = False

isFunction (FUNC _ _ _ _) = True
isFunction _ = False

data SemanticUnit = Root {
    scope :: [String]
}                   | SU {
    scope :: [String],
    kind :: Kind,
    symbolTable :: [Symbol],
    inheritFrom :: SemanticUnit
} deriving (Eq)

instance Show SemanticUnit where
  show (SU scope kind table from) = (show kind) ++ ": " ++ (show scope) ++ "\n" ++ (show table) ++ "\n"
  show (Root scope) = "ROOT: " ++ (show scope) ++ "\n"

buildSymbolFromConstructor (Cons constructorModifiers constructorName constructorParameters constructorInvocation constructorDefinition consi) = FUNC constructorModifiers constructorName (map typeName constructorParameters) (Object (Name [constructorName]))
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
buildEnvironment comp@(Comp pkg imps def cui) = case pkg of
                                            Nothing -> buildEnvironmentWithPackage ["unnamed package"] (Root []) comp
                                            Just cname -> buildEnvironmentWithPackage cname (Root []) comp

buildEnvironmentWithPackage [] parent unit = env
    where
        def = definition unit
        cname' = [[]]
        su = case def of
                (CLS mds nm ext imps cons flds mtds clsi)   -> SU cname' Package [CL mds nm (TypeClass (Name [nm])) unit] parent
                (ITF mds nm imps mtds itfi)                 -> SU cname' Package [IT mds nm (TypeClass (Name [nm])) unit] parent
        env = case def of
                (CLS mds nm ext imps cons flds mtds clsi)   -> buildEnvironmentFromClass parent (CLS mds nm ext imps cons flds mtds clsi)
                (ITF mds nm imps mtds itfi)                 -> buildEnvironmentFromInterface parent (ITF mds nm imps mtds itfi)

buildEnvironmentWithPackage (name:remain) parent unit = ENV su env
    where
        def = definition unit
        cname' = ((scope parent) ++ [name])
        su = case remain of
                [] -> case def of
                          (CLS mds nm ext imps cons flds mtds clsi)   -> SU cname' Package [CL mds nm (TypeClass (Name [nm])) unit] parent
                          (ITF mds nm imps mtds itfi)                 -> SU cname' Package [IT mds nm (TypeClass (Name [nm])) unit] parent
                _ -> SU cname' Package [] parent
        env = [buildEnvironmentWithPackage remain su unit]

buildEnvironmentFromClass parent (CLS mds nm ext imps cons flds mtds clsi) = env
    where
        cname' = ((scope parent) ++ [nm])
        syms = (map buildSymbolFromConstructor cons) ++ (map buildSymbolFromField flds) ++ (map buildSymbolFromMethod mtds)
        su = (SU cname' Class syms parent)
        flds' = map (buildEnvironmentFromField su) flds
        cons' = map (buildEnvironmentFromConstructor su) cons
        mtds' = map (buildEnvironmentFromMethod su) mtds
        env = ENV su (flds' ++ cons' ++ mtds')
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

buildEnvironmentFromConstructor :: SemanticUnit -> Constructor -> Environment
buildEnvironmentFromConstructor parent (Cons constructorModifiers constructorName constructorParameters constructorInvocation constructorDefinition consi) = ENV su ch
    where
        cname' = ((scope parent) ++ [constructorName])
        syms = (map buildSymbolFromParameter constructorParameters)
        su = (SU cname' Method syms parent)
        stmts = if isNothing constructorInvocation then [] else [Expr (fromJust constructorInvocation)]
        stmts' = if isNothing constructorDefinition then stmts else stmts ++(statements (fromJust constructorDefinition))
        ch = [buildEnvironmentFromStatements su stmts']

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
