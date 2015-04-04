module CodeConstruct where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Either

import           AST          (Expression, Type)
import qualified AST
import           Environment
import           TypeDatabase
import           TypeLinking
import           Inheritance


data FieldType = FT {
  fieldName :: String,
  fieldType :: Type,
  fieldSYM :: Symbol,
  fieldInit :: Maybe DFExpression,
  isStatic :: Bool
} deriving (Show)


--fieldTableOffset :: [FT] -> String -> Int
--fieldTableOffset []
--fieldTableOffset ft:

buildFieldType :: [(Symbol, Maybe DFExpression)] -> [FieldType]
buildFieldType st
  | null st = []
  | otherwise = case (head st) of
                  (sym@(SYM mds ls nm tp), mval) -> (FT nm tp sym mval (elem "static" mds)):remain
                  (FUNC _ _ _ _ _, _) -> remain
                  _ -> error "buildFieldType: not SYM or FUNC"
      where
          remain = buildFieldType $ tail st

data ClassConstruct = CC {
  className :: [String],
  classFields :: [FieldType],
  classSymbol :: Symbol,
  classMethods :: [MethodConstruct]
} -- deriving (Show)

instance Show ClassConstruct where
  show (CC cname cfield csym cmtds) = (show cname) ++ "\n" ++ (show cfield) ++ "\n" ++ (show cmtds)

data InstanceConstruct = IC {
  instanceType :: [String],
  instanceFields :: [FieldType]
} deriving (Show)

buildClassConstruct :: TypeNode -> [[String]] -> Environment -> Maybe ClassConstruct

buildClassConstruct db imps (ENV su@(SU _ Package _ _) ch) = buildClassConstruct db imps (head ch)
buildClassConstruct db imps (ENV su@(SU _ Interface _ _) _) = Nothing
buildClassConstruct db imps (ENV su@(SU cname Class st parent) ch) = Just (CC cname ft sym mtdc)
  where
    flds = [fld | fld@(Field sym mval) <- map (kind . semantic) ch]
    flds' = map (\(Field sym mval) -> if isJust mval
                                         then (sym, Just $ buildDFExpression db imps su [] (fromJust mval))
                                         else (sym, Nothing)) flds
    ft = buildFieldType flds'
    [sym] = symbolTable parent
    mtds = [(ENV (SU cname' (Method sym') st' parent') ch') | (ENV (SU cname' (Method sym') st' parent') ch') <- ch]
    mtdc = map (buildMethodConstruct db imps) mtds

------------------------------------------

data MethodConstruct = MC {
  methodName :: [String],
  --methodParameters :: [AST.TypedVar],
  methodSymbol :: Symbol,
  methodDefinition :: [DFStatement]
}--deriving (Show)

instance Show MethodConstruct where
  show (MC mname msym mdefs) = (show mname) ++ "\n" ++ (show mdefs) ++ "\n"

buildMethodConstruct :: TypeNode -> [[String]] -> Environment -> MethodConstruct
buildMethodConstruct db imps (ENV su@(SU cname (Method sym) _ _) ch) = MC cname sym stmts
  where
    stmts = if null ch then [] else buildDFStatement db imps [1] (head ch)

------------------------------------------

data DFStatement = DFIf {
  condition :: DFExpression,
  ifBlock   :: [DFStatement],
  elseBlock :: [DFStatement],
  nesting   :: [Int]
} | DFWhile {
  condition  :: DFExpression,
  whileBlock :: [DFStatement],
  nesting    :: [Int]
} | DFFor {
  initializer :: DFStatement,
  condition   :: DFExpression,
  finalizer   :: DFStatement,
  forBlock    :: [DFStatement],
  nesting     :: [Int]
} | DFBlock {
  block :: [DFStatement],
  nesting :: [Int]
} | DFExpr DFExpression | DFLocal DFExpression | DFReturn (Maybe DFExpression) deriving (Show)


buildDFStatement :: TypeNode -> [[String]] -> [Int] -> Environment -> [DFStatement]
buildDFStatement _ _ _ ENVE  = []

buildDFStatement db imps nesting (ENV (SU _ Statement _ _) ch) = (DFBlock block nesting):remain
  where
    stmtch = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    block = head stmtch
    remain = last stmtch

buildDFStatement db imps nesting (ENV su@(SU _ (Var expr) st _) ch) = (DFLocal dfexpr):remain
  where
    remain = head $ zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr
    --[(SYM _ _ nm _)] = st
    --newExpr = AST.Binary "=" (AST.ID (AST.Name [nm]) 0) expr 0
    --dfexpr = buildDFExpression db imps su [] newExpr

buildDFStatement db imps nesting (ENV su@(SU _ (Exp expr) _ _) ch) = (DFExpr dfexpr):remain
  where
    remain = head $ zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr


buildDFStatement db imps nesting (ENV su@(SU scope (IfBlock expr) _ _) ch) = (DFIf dfexpr ifpart elsepart nesting):remain
  where
    stmtch = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr
    ifpart = head stmtch
    elsepart = head $ tail stmtch
    remain = last stmtch


buildDFStatement db imps nesting (ENV su@(SU _ (Ret expr) _ _) ch) = (DFReturn dfexpr):remain
  where
    remain = head $ zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = if (isNothing expr) then Nothing else Just dfexpr'
    dfexpr' = buildDFExpression db imps su [] (fromJust expr)


buildDFStatement db imps nesting (ENV su@(SU _ (WhileBlock expr) _ _) ch) = (DFWhile dfexpr whilepart nesting):remain
  where
    stmtch = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr
    whilepart = head stmtch
    remain = last $ stmtch

buildDFStatement db imps nesting (ENV su@(SU _ ForBlock st _) ch) = (DFFor initstmt' expr forstmt forblock nesting):remain
  where
    initstmt = head ch
    [DFExpr initR] = buildDFStatement db imps (nesting ++ [1]) initstmt
    --[(SYM _ _ varL _)] = st
    --exprL = AST.ID (AST.Name [varL]) 0
    --initL = buildDFExpression db imps su [] exprL
    initstmt' = if null st then (DFExpr initR) else (DFLocal initR)

    stmtch = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [2..] (tail ch)
    DFExpr expr = head $ head stmtch
    forstmt = head $ head $ tail stmtch
    forblock = head $ tail $ tail stmtch
    remain = last $ stmtch

------------------------------------------

data DFExpression = FunctionCall Symbol [DFExpression]
                  | Unary { op :: String, expr :: DFExpression }
                  | Binary { op :: String, exprL :: DFExpression, exprR :: DFExpression }
                  | Attribute { struct :: DFExpression, mem :: Symbol }
                  | InstanceOf { reftype :: Type, expr :: DFExpression }
                  | Cast {reftype :: Type, expr :: DFExpression }
                  | ID { identifier :: Either Int Symbol }
                  | Value { valuetype :: Type, value :: String }
                  | Null
                  | NOOP
                  deriving (Show)


buildDFExpression :: TypeNode -> [[String]] -> SemanticUnit -> [Type] -> Expression -> DFExpression
-- trivial ones
buildDFExpression db imps su tps AST.Null = Null
buildDFExpression db imps su tps AST.This = ID (Left $ thisOffset su)
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
                                                                       then let offset = (scopeOffset su sym) in
                                                                         if offset >= 0 then ID (Left offset) else ID (Left $ offset - 2)
                                                                       else ID (Right sym)
  where
    --ls = case symbolLinkingName db imps su n of
            --[sym@(SYM _ ls' _ _)] -> ls'
            --err -> error $ "resolving ID " ++ (show e) ++ (show $  inheritFrom su)
    --sym = head $ symbolLinkingName db imps su n
    [sym@(SYM _ ls _ _)] = symbolLinkingName db imps su n
    baseName = (AST.typeToName . lookUpThis) su
buildDFExpression db imps su tps e@ (AST.ID n@(AST.Name cname@(nm:remain)) _) = if elem "static" (symbolModifiers sym)
                                                                                  then ID (Right sym)
                                                                                  else Attribute (buildDFExpression db imps su [tps'] (AST.ID (AST.Name (init cname)) 0)) sym
  where
    syms = symbolLinkingName db imps su n
    --[sym] = case if tps == [] then syms else [sym | sym <- syms, elem (symbolToType' sym) tps] of
     --         [sym'] -> [sym']
     --         err -> error $ "\n" ++ (show $ map symbolToType' syms) ++ "  " ++ (show tps)
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType' sym) tps]
    tps' = AST.Object (AST.Name (localScope sym))

buildDFExpression db imps su tps e@(AST.Attribute expr mem _) = Attribute (buildDFExpression db imps su [tps'] expr) sym
  where
    syms = symbolLinkingExpr db imps su e
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType' sym) tps]
    tps' = AST.Object (AST.Name (localScope sym))

-- replace with function calls
buildDFExpression db imps su tps e@(AST.FunctionCall expr args _) = if elem "static" (symbolModifiers sym)
                                                                      then FunctionCall sym $ exprL:(map (buildDFExpression db imps su []) args)
                                                                      else FunctionCall sym (map (buildDFExpression db imps su []) args)
  where
    syms = symbolLinkingExpr db imps su e
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType' sym) tps]
    tps' = symbolToType' sym
    exprL = case buildDFExpression db imps su [tps'] expr of
              Attribute exprL sym -> exprL
              ID _ -> ID (Left $ thisOffset su)


-- inject malloc calls
buildDFExpression db imps su tps e@(AST.NewObject tp args _) = FunctionCall sym $ (buildMalloc sym):(map (buildDFExpression db imps su []) args)
  where
    syms = symbolLinkingExpr db imps su e
    [sym] = if tps == [] then syms else [sym | sym <- syms, elem (symbolToType' sym) tps]

buildDFExpression db imps su tps (AST.NewArray tp expr _) = FunctionCall sym $ (buildMalloc sym):[buildDFExpression db imps su [] expr]
  where
    [sym] = symbolLinkingName db imps su (AST.Name ["joosc native", "Array", "Array"])
    --sym = symbol arrayConstructor

buildDFExpression db imps su tps (AST.Dimension dummy index _) = if dummy /= AST.Null then error $ "Dimension not NULL: " ++ (show dummy)
                                                                 else buildDFExpression db imps su [] index

buildDFExpression db imps su tps (AST.InstanceOf tp expr _) = InstanceOf tp (buildDFExpression db imps su [] expr)

-- noop
buildDFExpression db imps su tps (AST.ArrayAccess expr expri _) = FunctionCall sym $ [dfexpr, dfexpri]
    where
      [sym] = symbolLinkingName db imps su (AST.Name ["joosc native", "Array", "get"])
      dfexpr = buildDFExpression db imps su tps expr
      dfexpri = buildDFExpression db imps su tps expri

buildDFExpression db imps su tps (AST.Unary op expr _) = Unary op (buildDFExpression db imps su tps expr)
buildDFExpression db imps su tps (AST.Binary op exprL exprR _) = Binary op (buildDFExpression db imps su tps exprL) (buildDFExpression db imps su tps exprR)

buildDFExpression db imps su tps ow = error $ show ow


---------------------------------------------------------------


createClassInitOrdering :: [ClassConstruct] -> [[String]]
createClassInitOrdering tps = generateClassOrdering nodes edges
  where
    pairs = [(classSymbol tp, listStaticSYMFromStaticInit tp) | tp <- tps]
    nodes = map (symbolToCN . fst) pairs
    edges = generateEdgeFromPairs pairs

listStaticSYMFromStaticInit :: ClassConstruct -> [Symbol]
listStaticSYMFromStaticInit (CC cname ft sym mtdc) = concat $ map listStaticSYMFromDFExpr exprs
  where
    exprs = [expr | Just expr <- map fieldInit ft]


listStaticSYMFromDFExpr :: DFExpression -> [Symbol]
listStaticSYMFromDFExpr (FunctionCall _ exprs) = concat $ map listStaticSYMFromDFExpr exprs
listStaticSYMFromDFExpr (Unary _ expr) = listStaticSYMFromDFExpr expr
listStaticSYMFromDFExpr (Binary _ exprL exprR) = (listStaticSYMFromDFExpr exprL) ++ (listStaticSYMFromDFExpr exprR)
listStaticSYMFromDFExpr (Attribute expr sym) = listStaticSYMFromDFExpr expr
listStaticSYMFromDFExpr (InstanceOf _ expr) = listStaticSYMFromDFExpr expr
listStaticSYMFromDFExpr (Cast _ expr) = listStaticSYMFromDFExpr expr
listStaticSYMFromDFExpr (ID (Right sym)) = case sym of
                                                      SYM mds _ _ _ -> if (elem "static" mds) && (not $ elem "native" mds)
                                                                          then [sym]
                                                                          else []
                                                      _ -> []
listStaticSYMFromDFExpr _ = []

---------------------------------------------------------------

buildMalloc sym = FunctionCall sym' [arg]
  where
    sym' = symbol runtimeMalloc
    arg = ID $ Right (SYM ["static", "native"] (localScope sym) "object size" AST.TypeInt)


symbolToType' :: Symbol -> Type
symbolToType' sym = case tp of
                      AST.Array _ -> AST.Object (AST.Name ["joosc native","Array"])
                      ow -> ow
  where
    tp = symbolToType sym


--     _                           _     _
--    / \   ___ ___  ___ _ __ ___ | |__ | |_   _
--   / _ \ / __/ __|/ _ \ '_ ` _ \| '_ \| | | | |
--  / ___ \\__ \__ \  __/ | | | | | |_) | | |_| |
-- /_/   \_\___/___/\___|_| |_| |_|_.__/|_|\__, |
--                                         |___/
--   ____                           _   _
--  / ___| ___ _ __   ___ _ __ __ _| |_(_) ___  _ __
-- | |  _ / _ \ '_ \ / _ \ '__/ _` | __| |/ _ \| '_ \
-- | |_| |  __/ | | |  __/ | | (_| | |_| | (_) | | | |
--  \____|\___|_| |_|\___|_|  \__,_|\__|_|\___/|_| |_|

recoverStackForBlock :: [DFStatement] -> [String]
recoverStackForBlock stmts = replicate counter "pop eax ; exit from block"
  where
    counter = length $ [ 1 | (DFLocal _) <- stmts]


genLabel :: [Int] -> String
genLabel nesting = concat $ intersperse "_" $ map show nesting

genAsm :: ClassConstruct -> [String]
genAsm (CC name fields _ methods) =
  let prefaceCode = ["; Code for: " ++ concat name, "section .text"]
      fieldCode = concat $ map genFieldAsm fields
      methodCode = concat $ map genMthdAsm methods
  in prefaceCode ++ fieldCode ++ methodCode

genFieldAsm :: FieldType -> [String]
genFieldAsm (FT _ _ _ _ False) = []
genFieldAsm fld@(FT name _ _ _ True) = ["; Class field: " ++ (show fld)]

genMthdAsm :: MethodConstruct -> [String]
genMthdAsm (MC name symbol definition) =
  let label = generateLabelFromFUNC symbol 0
      header =  ["global " ++ label, label ++ ":", "; Start a new stack frame", "push ebp", "mov ebp, esp"]
      body = concat $ map genStmtAsm definition
  in header ++ body ++ ["; End of " ++ last name]

genStmtAsm :: DFStatement -> [String]
genStmtAsm (DFExpr expr) = genExprAsm expr
genStmtAsm (DFLocal expr) = genExprAsm expr ++ ["push eax ; Allocating stack space for local var"]
genStmtAsm (DFReturn Nothing) = ["; Void return", "ret"]
genStmtAsm (DFReturn (Just retVal)) = ["; Value return"] ++ genExprAsm retVal ++ ["; Cleaning stack frame", "mov esp, ebp", "pop ebp", "ret"];
genStmtAsm (DFBlock body nesting) =
  let bodyCode = concat $ map genStmtAsm body
      recoveryCode = recoverStackForBlock body
  in ["; new block"] ++ bodyCode ++ recoveryCode

genStmtAsm (DFIf cond trueBlock falseBlock nesting) =
  let condCode = genExprAsm cond
      trueCode = concat $ map genStmtAsm trueBlock
      falseCode = concat $ map genStmtAsm falseBlock
      falseLabel = ".falsePart_" ++ genLabel nesting
      endLabel = ".end_" ++ genLabel nesting
  in ["; If statement"] ++
     condCode ++
     ["; If statement comparison", "cmp eax, 1", "jne " ++ falseLabel ++ "; If statement true case"] ++
     trueCode ++
     ["jmp " ++ endLabel, "; If statement false case", falseLabel ++ ":"] ++
     falseCode ++
     [endLabel ++ ":"]


genStmtAsm (DFWhile cond body nesting) =
  let condCode = genExprAsm cond
      bodyCode = concat $ map genStmtAsm body
      topLabel = ".whileCond_" ++ genLabel nesting
      bottomLabel = ".whileBottom_" ++ genLabel nesting
  in [topLabel ++ ":", "; While statement"] ++
     condCode ++
     ["cmp eax, 1", "jne " ++ bottomLabel] ++
     bodyCode ++
     ["jmp " ++ topLabel] ++
     [bottomLabel ++ ":"]

genStmtAsm (DFFor initializer condition finalizer body nesting) =
  let initializerCode = genStmtAsm initializer
      conditionCode = genExprAsm condition
      finalizerCode = genStmtAsm finalizer
      bodyCode = concat $ map genStmtAsm body
      topLabel = ".forCond_" ++ genLabel nesting
      bottomLabel = ".forBottom_" ++ genLabel nesting
      recoverCode = recoverStackForBlock [initializer]
  in ["; For statement init"] ++
     initializerCode ++
     [topLabel ++ ": ; For condition evaluation"] ++
     conditionCode ++
     ["cmp eax, 1", "jne " ++ bottomLabel] ++
     bodyCode ++
     finalizerCode ++
     ["jmp " ++ topLabel] ++
     [bottomLabel ++ ":"] ++
     recoverCode


genOpAsm :: String -> [String]
genOpAsm "*" = ["imul ebx"]
genOpAsm "/" = ["cdq", "idiv ebx"]
genOpAsm "%" = ["cdq", "idiv ebx", "mov eax, edx"]
genOpAsm "+" = ["add eax, ebx"]
genOpAsm "-" = ["sub eax, ebx"]
genOpAsm "==" = ["cmp eax, ebx", "mov eax, 1", "je short $+7", "mov eax, 0"]
genOpAsm "!=" = ["cmp eax, ebx", "mov eax, 1", "jne short $+7", "mov eax, 0"]
genOpAsm "<" = ["cmp eax, ebx", "mov eax, 1", "jl short $+7", "mov eax, 0"]
genOpAsm "<=" = ["cmp eax, ebx", "mov eax, 1", "jle short $+7", "mov eax, 0"]
genOpAsm ">" = ["cmp eax, ebx", "mov eax, 1", "jg short $+7", "mov eax, 0"]
genOpAsm ">=" = ["cmp eax, ebx", "mov eax, 1", "jge short $+7", "mov eax, 0"]
genOpAsm "&&" = ["and eax, ebx"]
genOpAsm "||" = ["or eax, ebx"]
genOpAsm "=" = ["mov [eax], ebx"]

genExprAsm :: DFExpression -> [String]

genExprAsm (FunctionCall callee arguments) =
  let argumentCode = ((intersperse "push eax") . concat $ map genExprAsm arguments) ++ ["push eax"]
      cleanupCode = ["add esp, " ++ (show $ 4 * (length arguments)) ++ " ; Pop arguments"]
  in ["; Function call to" ++ localName callee, "; arguments"] ++
     argumentCode ++
     ["call _" ++ (localName callee)] ++
     cleanupCode

genExprAsm (Unary op expr) =
  let exprCode = genExprAsm expr
  in [";Unary op: " ++ op] ++ exprCode

genExprAsm (Binary op exprL exprR) =
  let
      rightCode = genExprAsm exprR
      opCode = genOpAsm op
      leftCode = case op of
        "=" -> genExprLhsAsm exprL
        _ -> genExprAsm exprL
  in [";Binary op: " ++ op] ++
     rightCode ++
     ["push eax ; Push right value to stack"] ++
     leftCode ++
     ["pop ebx ; Pop right value from stack"] ++
     opCode

genExprAsm (Attribute struct member) =
  let structCode = genExprAsm struct
  in [";Attribute"] ++ structCode
{-
genExprAsm (ArrayAccess array index) =
  let arrayCode = genExprAsm array
      indexCode = genExprAsm index
  in ["; newArray",";array"] ++ arrayCode ++ [";index"] ++ indexCode
-}
{-
genExprAsm (NewArray arrayType dimExpr) = ["; newArray"] ++ genExprAsm dimExpr
genExprAsm (NewObject classType args) =
  let argCode = concat $ map genExprAsm args
  in ["; newObject"] ++ argCode
-}
genExprAsm (InstanceOf refType expr) = ["; instanceOf"] ++ genExprAsm expr
genExprAsm (Cast refType expr) = ["; Casting"] ++ genExprAsm expr
genExprAsm (ID (Right symbol)) = ["; variable named " ++ (localName symbol)]
genExprAsm (ID (Left offset)) =
  let distance = (offset + 1) * 4
  in ["mov eax, [ebp - " ++ show distance ++ "];" ++ show offset]
genExprAsm (Value AST.TypeByte value) = ["mov eax, " ++ value]
genExprAsm (Value AST.TypeShort value) = ["mov eax, " ++ value]
genExprAsm (Value AST.TypeInt value) = ["mov eax, " ++ value]
genExprAsm (Value AST.TypeChar value) =
  let charValue = show . ord $ (read value :: Char)
  in ["mov eax, " ++ charValue]
genExprAsm (Value AST.TypeBoolean "true") = ["mov eax, 1"]
genExprAsm (Value AST.TypeBoolean "false") = ["mov eax, 0"]
genExprAsm (Value AST.TypeNull _) = ["mov eax, 0"]
genExprAsm (Value valuetype value) = ["; XXX: Unsupported value: " ++ value]
--genExprAsm This = ["mov eax, 0; This"]
genExprAsm Null = ["; Null"]
genExprAsm NOOP = ["; NOOP"]

genExprLhsAsm (ID (Left offset)) =
  let distance = (offset + 1) * 4
  in ["lea eax, [ebp - " ++ show distance ++ "] ; LHS for assignment"]
genExprLhsAsm (ID (Right symbol)) = ["; LHS Right symbol for assignment"]
