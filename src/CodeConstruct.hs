module CodeConstruct where

import           Data.Maybe
import           Data.Either

import           AST          (Expression, Type)
import qualified AST
import           Environment
import           TypeDatabase
import           TypeLinking


data FieldType = FT {
  fieldName :: String,
  fieldType :: Type,
  isStatic :: Bool
}

--fieldTableOffset :: [FT] -> String -> Int
--fieldTableOffset [] 
--fieldTableOffset ft:

buildFieldType :: [Symbol] -> [FieldType]
buildFieldType st
  | null st = []
  | otherwise = case (head st) of
                  SYM mds ls nm tp -> (FT nm tp (elem "static" mds)):remain
                  FUNC _ _ _ _ _ -> remain
                  _ -> error "buildFieldType: not SYM or FUNC"
      where
          remain = buildFieldType $ tail st

data ClassConstruct = CC {
  className :: [String],
  classFields :: [FieldType],
  classSymbol :: Symbol,
  classMethods :: [MethodConstruct]
}

buildClassConstruct :: TypeNode -> [[String]] -> Environment -> ClassConstruct

buildClassConstruct db imps (ENV su@(SU cname Class st parent) ch) = CC cname ft sym mtdc
  where
    ft = buildFieldType st
    [sym] = symbolTable parent
    mtds = [(ENV (SU cname' (Method sym') st' parent') ch') | (ENV (SU cname' (Method sym') st' parent') ch') <- ch]
    mtdc = map (buildMethodConstruct db imps) mtds

------------------------------------------

data MethodConstruct = MC {
  methodName :: [String],
  --methodParameters :: [AST.TypedVar],
  methodSymbol :: Symbol,
  methodDefinition :: [DFStatement]
}

buildMethodConstruct :: TypeNode -> [[String]] -> Environment -> MethodConstruct
buildMethodConstruct db imps (ENV su@(SU cname (Method sym) _ _) ch) = MC cname sym stmts
  where
    stmts = buildDFStatement db imps (head ch)

------------------------------------------

data InstanceConstruct = IC {
  instanceType :: [String],
  instanceFields :: [FieldType]
}


------------------------------------------

data DFStatement = DFIf {
  condition :: DFExpression,
  ifBlock   :: [DFStatement],
  elseBlock :: [DFStatement]
} | DFWhile {
  condition  :: DFExpression,
  whileBlock :: [DFStatement]
} | DFFor {
  initializer :: DFStatement,
  condition   :: DFExpression,
  finalizer   :: DFStatement,
  forBlock    :: [DFStatement]
} | DFExpr DFExpression | DFReturn (Maybe DFExpression) | DFBlock [DFStatement]


buildDFStatement :: TypeNode -> [[String]] -> Environment -> [DFStatement]
buildDFStatement db imps ENVE = []

buildDFStatement db imps (ENV (SU _ Statement _ _) ch) = (DFBlock block):remain
  where
    stmtch = map (buildDFStatement db imps) ch
    block = head stmtch
    remain = last stmtch


buildDFStatement db imps (ENV su@(SU _ (Var expr) st _) ch) = (DFExpr dfexpr):remain
  where
    remain = head $ map (buildDFStatement db imps) ch
    [(SYM _ _ nm _)] = st
    newExpr = AST.Binary "=" (AST.ID (AST.Name [nm]) 0) expr 0
    dfexpr = buildDFExpression db imps su [] newExpr

buildDFStatement db imps (ENV su@(SU _ (Exp expr) _ _) ch) = (DFExpr dfexpr):remain
  where
    remain = head $ map (buildDFStatement db imps) ch
    dfexpr = buildDFExpression db imps su [] expr


buildDFStatement db imps (ENV su@(SU _ (IfBlock expr) _ _) ch) = (DFIf dfexpr ifpart elsepart):remain
  where
    stmtch = map (buildDFStatement db imps) ch
    dfexpr = buildDFExpression db imps su [] expr
    ifpart = head stmtch
    elsepart = head $ tail stmtch
    remain = last stmtch


buildDFStatement db imps (ENV su@(SU _ (Ret expr) _ _) ch) = (DFReturn dfexpr):remain
  where
    remain = head $ map (buildDFStatement db imps) ch
    dfexpr = if (isNothing expr) then Nothing else Just dfexpr'
    dfexpr' = buildDFExpression db imps su [] (fromJust expr)


buildDFStatement db imps (ENV su@(SU _ (WhileBlock expr) _ _) ch) = (DFWhile dfexpr whilepart):remain
  where
    stmtch = map (buildDFStatement db imps) ch
    dfexpr = buildDFExpression db imps su [] expr
    whilepart = head stmtch
    remain = last $ stmtch

buildDFStatement db imps (ENV (SU _ ForBlock _ _) ch) = (DFFor initstmt expr forstmt forblock):remain
  where
    stmtch = map (buildDFStatement db imps) ch
    initstmt = head $ head stmtch
    DFExpr expr = head $ head $ tail stmtch
    forstmt = head $ head $ tail $ tail stmtch
    forblock = head $ tail $ tail $ tail stmtch
    remain = last $ stmtch

------------------------------------------

data DFExpression = FunctionCall Symbol [DFExpression]
                  | Unary { op :: String, expr :: DFExpression }
                  | Binary { op :: String, exprL :: DFExpression, exprR :: DFExpression }
                  | Attribute { struct :: DFExpression, mem :: Symbol }
                  | ArrayAccess { array :: DFExpression, index :: DFExpression }
                  | NewArray { arraytype :: Type, dimexprs :: DFExpression }
                  | NewObject { classtype :: Type, arguments :: [DFExpression] }
                  | InstanceOf { reftype :: Type, expr :: DFExpression }
                  | Cast {reftype :: Type, expr :: DFExpression }
                  | ID { identifier :: Either Int Symbol }
                  | Value { valuetype :: Type, value :: String }
                  | This
                  | Null
                  | NOOP


buildDFExpression :: TypeNode -> [[String]] -> SemanticUnit -> [Type] -> Expression -> DFExpression
-- trivial ones
buildDFExpression db imps su tps AST.Null = Null
buildDFExpression db imps su tps AST.This = This
buildDFExpression db imps su tps (AST.Value t v _) = Value t v
buildDFExpression db imps su tps e@(AST.CastA _ _ expr _) = Cast tp (buildDFExpression db imps su tps expr)
  where
    [tp] = typeLinkingExpr db imps su e
buildDFExpression db imps su tps e@(AST.CastB _ expr _) = Cast tp (buildDFExpression db imps su tps expr)
  where
    [tp] = typeLinkingExpr db imps su e
buildDFExpression db imps su tps e@(AST.CastC _ _ expr _) = Cast tp (buildDFExpression db imps su tps expr)
  where
    [tp] = typeLinkingExpr db imps su e
-- with modification to be more specific
buildDFExpression db imps su tps e@(AST.ID n@(AST.Name cname@[nm]) _) = if (init ls) == baseName
                                                                       then ID (Left (scopeOffset su sym))
                                                                       else ID (Right sym)
  where
    [sym@(SYM _ ls _ _)] = symbolLinkingName db imps su n
    baseName = (AST.typeToName . lookUpThis) su
buildDFExpression db imps su tps e@ (AST.ID n@(AST.Name cname@(nm:remain)) _) = if elem "static" (symbolModifiers sym)
                                                                                  then ID (Right sym)
                                                                                  else Attribute (buildDFExpression db imps su [tps'] (AST.ID (AST.Name remain) 0)) sym
  where
    syms = symbolLinkingName db imps su n
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType sym) tps]
    tps' = AST.Object (AST.Name (localScope sym))

buildDFExpression db imps su tps e@(AST.Attribute expr mem _) = Attribute (buildDFExpression db imps su [tps'] expr) sym
  where
    syms = symbolLinkingExpr db imps su e
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType sym) tps]
    tps' = AST.Object (AST.Name (localScope sym))

-- replace with function calls
buildDFExpression db imps su tps e@(AST.FunctionCall expr args _) = if elem "static" (symbolModifiers sym)
                                                                      then FunctionCall sym $ exprL:(map (buildDFExpression db imps su []) args)
                                                                      else FunctionCall sym (map (buildDFExpression db imps su []) args)
  where
    syms = symbolLinkingExpr db imps su e
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType sym) tps]
    tps' = symbolToType sym
    exprL = case buildDFExpression db imps su [tps'] expr of
              Attribute exprL sym -> exprL
              ID _ -> This


-- inject malloc calls
buildDFExpression db imps su tps e@(AST.NewObject tp args _) = FunctionCall sym $ (buildMalloc sym):(map (buildDFExpression db imps su []) args)
  where
    syms = symbolLinkingExpr db imps su e
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType sym) tps]
              
buildDFExpression db imps su tps (AST.NewArray tp expr _) = FunctionCall sym $ (buildMalloc sym):[buildDFExpression db imps su [] expr] 
  where
    sym = symbol arrayConstructor
-- noop
buildDFExpression db imps su tps (AST.ArrayAccess expr expri _) = ArrayAccess (buildDFExpression db imps su tps expr) (buildDFExpression db imps su tps expri)
buildDFExpression db imps su tps (AST.Unary op expr _) = Unary op (buildDFExpression db imps su tps expr)
buildDFExpression db imps su tps (AST.Binary op exprL exprR _) = Binary op (buildDFExpression db imps su tps exprL) (buildDFExpression db imps su tps exprR)

buildMalloc sym = FunctionCall sym' [arg]
  where
    sym' = symbol runtimeMalloc
    arg = ID $ Right (SYM ["static", "native"] (localScope sym) "object size" AST.TypeInt)

