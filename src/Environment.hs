module Environment where

import Data.Maybe
import AST

data Kind = Class | Interface | Method | Field | Statement | Var Expression | Exp Expression | Ret Expression | WhileBlock Expression | IfBlock Expression | ForBlock deriving (Show)

data Symbol = SYM {
    localName :: String,
    localType :: Type
}           | FUNC {
    localName :: String,
    parameterTypes :: [Type],
    localType :: Type
} deriving (Show)

data SemanticUnit = Root | SU {
    scope :: [String],
    kind :: Kind,
    symbolTable :: [Symbol],
    inheritFrom :: SemanticUnit
} deriving (Show)

buildSymbolFromConstructor (Cons constructorModifiers constructorName constructorParameters constructorInvocation constructorDefinition consi) = FUNC constructorName (map typeName constructorParameters) (Object (Simple constructorName))
buildSymbolFromField (FLD fieldModifiers fieldVar fieldValue fldi) = buildSymbolFromParameter fieldVar
buildSymbolFromMethod (MTD methodModifiers (TV tp nm ai) methodParameters methodDefinition mtdi) = FUNC nm (map typeName methodParameters) tp
buildSymbolFromParameter (TV tp nm ai) = SYM nm tp

data Environment = ENVE | ENV {
    semantic :: SemanticUnit,
    children :: [Environment]
} deriving (Show)

buildEnvironment :: CompilationUnit -> Environment
buildEnvironment (Comp pkg imps def cui) = case def of
                                            (CLS mds nm ext imps cons flds mtds clsi)   -> buildEnvironmentFromClass cname (CLS mds nm ext imps cons flds mtds clsi)
                                            (ITF mds nm imps mtds itfi)                 -> buildEnvironmentFromInterface cname (ITF mds nm imps mtds itfi)
    where
        cname = if isNothing pkg then [] else fromJust pkg

buildEnvironmentFromClass cname (CLS mds nm ext imps cons flds mtds clsi) = ENV su (flds' ++ mtds')
    where
        cname' = (cname ++ [nm])
        syms = (map buildSymbolFromConstructor cons) ++ (map buildSymbolFromField flds) ++ (map buildSymbolFromMethod mtds)
        su = (SU cname' Class syms Root)
        flds' = map (buildEnvironmentFromField su) flds
        mtds' = map (buildEnvironmentFromMethod su) mtds
buildEnvironmentFromInterface cname (ITF mds nm imps mtds itfi) = ENV su (mtds')
    where
        cname' = (cname ++ [nm])
        syms = (map buildSymbolFromMethod mtds)
        su = (SU cname' Interface syms Root)
        mtds' = map (buildEnvironmentFromMethod su) mtds

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
        
