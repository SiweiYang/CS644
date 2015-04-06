module CodeConstruct where

import           Prelude hiding (lookup)
import           Data.Map (Map, (!), lookup, assocs, toAscList)
import           Data.Char
import           Data.List hiding (lookup)
import           Data.Maybe
import           Data.Either

import           AST          (Expression, Type)
import qualified AST
import           Environment
import           TypeDatabase
import           TypeLinking
import           Inheritance
import           TypeChecking


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

{-
data InstanceConstruct = IC {
  instanceType :: [String],
  instanceFields :: [FieldType]
} deriving (Show)
-}

objectInitializer :: ClassConstruct -> [DFExpression]
objectInitializer (CC _ flds sym _) = map newexpr nonstatic
  where
    nonstatic = filter (not . isStatic) flds
    capsule = \sym -> ID $ Right (-1, sym)
    initialTPvalue = \tp -> if isPrimitive tp then (Value AST.TypeInt "0") else Null
    initialvalue = \expr tp -> if isNothing expr then initialTPvalue tp else fromJust expr
    newexpr = \(FT _ tp sym expr _) -> Binary "=" (capsule sym) (initialvalue expr tp)

classInitializer :: ClassConstruct -> [DFExpression]
classInitializer (CC _ flds sym _) = map newexpr static
  where
    static = filter isStatic flds
    capsule = \sym -> ID $ Right (0, sym)
    initialTPvalue = \tp -> if isPrimitive tp then (Value AST.TypeInt "0") else Null
    initialvalue = \expr tp -> if isNothing expr then initialTPvalue tp else fromJust expr
    newexpr = \(FT _ tp sym expr _) -> Binary "=" (capsule sym) (initialvalue expr tp)

---------------------------------------------------------------

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
    [block, remain] = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch

buildDFStatement db imps nesting (ENV su@(SU _ (Var expr) st _) ch) = (DFLocal dfexpr):remain
  where
    [remain] = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr

buildDFStatement db imps nesting (ENV su@(SU _ (Exp expr) _ _) ch) = (DFExpr dfexpr):remain
  where
    [remain] = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr


buildDFStatement db imps nesting (ENV su@(SU scope (IfBlock expr) _ _) ch) = (DFIf dfexpr ifpart elsepart nesting):remain
  where
    [ifpart, elsepart, remain] = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr

buildDFStatement db imps nesting (ENV su@(SU _ (Ret expr) _ _) ch) = (DFReturn dfexpr):remain
  where
    [remain] = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = if scopeConstructor su
                then Just (buildDFExpression db imps su [] (AST.This))
                else if (isNothing expr) then Nothing else Just dfexpr'
    dfexpr' = buildDFExpression db imps su [] (fromJust expr)


buildDFStatement db imps nesting (ENV su@(SU _ (WhileBlock expr) _ _) ch) = (DFWhile dfexpr whilepart nesting):remain
  where
    [whilepart, remain] = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    dfexpr = buildDFExpression db imps su [] expr

buildDFStatement db imps nesting (ENV su@(SU _ ForBlock st _) ch) = (DFFor initstmt expr' forstmt' forblock nesting):remain
  where
    [initializer, cond, finalizer, forblock, remain] = zipWith (\num child -> buildDFStatement db imps (nesting ++ [num]) child) [1..] ch
    [DFExpr initR] = initializer
    initstmt = if null initializer then (DFExpr NOOP) else
               if null st then (DFExpr initR) else (DFLocal initR)
    [DFExpr expr] = cond
    expr' = if null cond then NOOP else expr
    [forstmt] = finalizer
    forstmt' = if null finalizer then (DFExpr NOOP) else forstmt

------------------------------------------

data DFExpression = FunctionCall Symbol [DFExpression]
                   | ArrayAccess Symbol DFExpression DFExpression
                   | Unary { op :: String, expr :: DFExpression }
                   | Binary { op :: String, exprL :: DFExpression, exprR :: DFExpression }
                   | Attribute { struct :: DFExpression, mem :: Symbol }
                   | InstanceOf { refsym :: Symbol, expr :: DFExpression }
                   | Cast {reftype :: Either Type Symbol, expr :: DFExpression }
                   | ID { identifier :: Either Int (Int, Symbol) }
                   | Value { valuetype :: Type, value :: String }
                   | Super { offset :: Int, super :: Maybe Symbol }
                   | Label String
                   | Null
                   | NOOP
                   deriving (Show)


buildDFExpression :: TypeNode -> [[String]] -> SemanticUnit -> [Type] -> Expression -> DFExpression
-- trivial ones
buildDFExpression db imps su tps AST.Null = Null
buildDFExpression db imps su tps AST.This = ID (Left $ thisOffset su)
buildDFExpression db imps su tps (AST.Super msuper) = case msuper of
                                                        Nothing -> Super (thisOffset su) Nothing
                                                        Just nm -> let [tp] = take 1 (traverseTypeEntryWithImports db imps nm)
                                                                       Just tn = getTypeEntry db tp
                                                                       [con] = [sym | sym@(FUNC mds _ _ pt _) <- map symbol $ subNodes tn, elem "cons" mds, pt == []]
                                                                       [sym] = getSymbol db tp
                                                                   in Super (thisOffset su) (Just con)
buildDFExpression db imps su tps (AST.Value t v _) = Value t v
buildDFExpression db imps su tps e@(AST.CastA _ _ expr _) = Cast tpsym (buildDFExpression db imps su [] expr)
  where
    [tp] = typeLinkingExpr db imps su e
    tpname = AST.Name $ AST.typeToName tp
    [sym] = symbolLinkingName db imps su tpname
    tpsym = if isPrimitive tp then Left tp else Right sym
buildDFExpression db imps su tps e@(AST.CastB _ expr _) = Cast tpsym (buildDFExpression db imps su [] expr)
  where
    [tp] = typeLinkingExpr db imps su e
    tpname = AST.Name $ AST.typeToName tp
    [sym] = symbolLinkingName db imps su tpname
    tpsym = if isPrimitive tp then Left tp else Right sym
buildDFExpression db imps su tps e@(AST.CastC _ _ expr _) = Cast tpsym (buildDFExpression db imps su [] expr)
  where
    [tp] = typeLinkingExpr db imps su e
    tpname = AST.Name $ AST.typeToName tp
    [sym] = symbolLinkingName db imps su tpname
    tpsym = if isPrimitive tp then Left tp else Right sym
-- with modification to be more specific
buildDFExpression db imps su tps e@(AST.ID n@(AST.Name cname@[nm]) _) = if take (length baseName) (init (localScope sym)) == baseName
                                                                        then let offset = (scopeOffset su sym) in ID $ Left offset
                                                                         --if offset >= 0 then ID (Left offset) else ID (Left offset)
                                                                        else ID $ Right (offthis, sym)
  where
    offthis = thisOffset su
    syms = symbolLinkingName db imps su n
    sym = case if tps == [] then syms else [sym | sym <- syms, elem (symbolToType' sym) tps] of
            [sym] -> sym
            err -> error $ (show e) ++ "cands after filter: " ++ (show err) ++ "cands: " ++ (show (map symbolToType' syms)) ++ "constriants: " ++ (show tps)
    {-
    [sym@(SYM _ ls _ _)] = case symbolLinkingName db imps su n of
                             [sym@ (SYM _ _ _ _)] -> [sym]
                             err -> error $ show su
    -}
    baseName = (AST.typeToName . lookUpThis) su
buildDFExpression db imps su tps e@ (AST.ID n@(AST.Name cname@(nm:remain)) _) = if elem "static" (symbolModifiers sym)
                                                                                  then ID $ Right (thisOffset su, sym)
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
                                                                      then FunctionCall sym (map (buildDFExpression db imps su []) args)
                                                                      else FunctionCall sym $ exprL:(map (buildDFExpression db imps su []) args)
  where
    syms = symbolLinkingExpr db imps su e
    sym = case if tps == [] then syms else [sym | sym@(FUNC _ _ _ _ rt) <- syms, elem (typeRelax rt) tps] of
                 [sym] -> sym
                 err -> error $ (show e) ++ "cands after filter: " ++ (show err) ++ "cands: " ++ (show (map symbolToType' syms)) ++ "constriants: " ++ (show tps)
    tps' = symbolToType' sym
    exprL = case buildDFExpression db imps su [tps'] expr of
              Attribute exprL sym -> exprL
              ID _ -> ID $ Left (thisOffset su)


-- inject malloc calls
buildDFExpression db imps su tps e@(AST.NewObject tp args _) = FunctionCall sym $ (buildMalloc sym):(map (buildDFExpression db imps su []) args)
  where
    syms = symbolLinkingExpr db imps su e
    --if tps == [] then syms else [sym | sym <- syms, elem (symbolToType' sym) tps]
    sym = case syms of
            [sym] -> sym
            err -> error $ (show err) ++ (show syms) ++ (show tps)

buildDFExpression db imps su tps (AST.NewArray tp expr _) = FunctionCall sym $ (buildMalloc sym):[buildDFExpression db imps su [] expr]
  where
    [sym, _] = getSymbol db ["joosc native", "Array", "Array"]
    --sym = symbol arrayConstructor

buildDFExpression db imps su tps (AST.Dimension dummy index _) = if dummy /= AST.Null then error $ "Dimension not NULL: " ++ (show dummy)
                                                                 else buildDFExpression db imps su [] index

buildDFExpression db imps su tps (AST.InstanceOf tp expr _) = InstanceOf sym (buildDFExpression db imps su [] expr)
  where
    nm = case tp of
           AST.Object nm -> nm
           AST.Array _ -> AST.Name ["joosc native", "Array"]
    syms = symbolLinkingName db imps su nm
    sym = case syms of
            [sym'] -> sym'
            err -> error $ (show err) ++ (show tp)


-- noop
buildDFExpression db imps su tps (AST.ArrayAccess expr expri _) = ArrayAccess sym dfexpr dfexpri
    where
      sym = case getSymbol db ["joosc native", "Array", "get"] of
                [sym'] -> sym'
                [] -> error $ "buildDFExpression Empty lookup Name" ++ (show db)
      tps' = case tps of
               --[tp] -> AST.Array tp
               _ -> AST.Object (AST.Name ["joosc native","Array"])
      dfexpr = buildDFExpression db imps su [tps'] expr
      dfexpri = buildDFExpression db imps su [] expri

buildDFExpression db imps su tps (AST.Unary op expr _) = Unary op (buildDFExpression db imps su tps expr)
buildDFExpression db imps su tps (AST.Binary op exprL exprR _) = Binary op (buildDFExpression db imps su [] exprL) (buildDFExpression db imps su [] exprR)

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
listStaticSYMFromDFExpr (ID (Right (_, sym))) = case sym of
                                                    SYM mds _ _ _ -> if (elem "static" mds) && (not $ elem "native" mds) then [sym] else []
                                                    _ -> []
listStaticSYMFromDFExpr _ = []

listStringFromClass :: ClassConstruct -> [String]
listStringFromClass (CC cname ft sym mtdc) = flds ++ mtds
  where
    flds = concat [listStringFromDFExpr expr | Just expr <- map fieldInit ft]
    mtds = concat $ map listStringFromStatement $ concat $ map methodDefinition mtdc

listStringFromStatement :: DFStatement -> [String]
listStringFromStatement (DFIf expr stmts1 stmts2 _) = (listStringFromDFExpr expr) ++ (concat $ map listStringFromStatement (stmts1 ++ stmts2))
listStringFromStatement (DFWhile expr stmts _) = (listStringFromDFExpr expr) ++ (concat $ map listStringFromStatement (stmts))
listStringFromStatement (DFFor stmt1 expr stmt2 stmts _) = (listStringFromDFExpr expr) ++ (concat $ map listStringFromStatement (stmt1:stmt2:stmts))
listStringFromStatement (DFBlock stmts _) = concat $ map listStringFromStatement stmts
listStringFromStatement (DFExpr expr) = listStringFromDFExpr expr
listStringFromStatement (DFLocal expr) = listStringFromDFExpr expr
listStringFromStatement (DFReturn mexpr) = case mexpr of
                                             Just expr -> listStringFromDFExpr expr
                                             Nothing -> []

listStringFromDFExpr :: DFExpression -> [String]
listStringFromDFExpr (FunctionCall _ exprs) = concat $ map listStringFromDFExpr (exprs)
listStringFromDFExpr (Unary _ expr) = listStringFromDFExpr expr
listStringFromDFExpr (Binary _ exprL exprR) = (listStringFromDFExpr exprL) ++ (listStringFromDFExpr exprR)
listStringFromDFExpr (Attribute expr sym) = listStringFromDFExpr expr
listStringFromDFExpr (InstanceOf _ expr) = listStringFromDFExpr expr
listStringFromDFExpr (Cast _ expr) = listStringFromDFExpr expr
listStringFromDFExpr (Value tp val) = if tp == AST.Object (AST.Name ["java", "lang", "String"])
                                         then [val]
                                         else []
listStringFromDFExpr _ = []
---------------------------------------------------------------

buildMalloc sym = FunctionCall sym' [arg]
  where
    sym' = symbol runtimeMalloc
    arg = ID $ Right (0, (SYM ["static", "native"] (localScope sym) "object size" AST.TypeInt)) -- ??? this offset

typeRelax tp = case tp of
                   AST.Array _ -> AST.Object (AST.Name ["joosc native","Array"])
                   ow -> ow

symbolToType' :: Symbol -> Type
symbolToType' sym = typeRelax tp
  where
    tp = symbolToType sym

data SymbolDatabase = SD {
  db :: TypeNode,
  funcLabel :: Map Symbol String,
  instanceSYMOffsetMap :: Map Symbol Int,
  instanceFUNCOffsetMap :: Map Symbol Int,
  staticSYMLabelMap :: Map Symbol String,
  instanceFUNCTable :: [(Symbol, [Maybe Symbol])],
  typeIDMap :: Map Symbol Int,
  stringLabel :: Map String String
}

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

genAsm :: SymbolDatabase -> ClassConstruct -> [String]
genAsm sd cc@(CC name fields sym methods) = ["; Code for: " ++ concat name]
                                            ++ externCode ++ ["; Code for virtual table", "section .data"] ++ classIDCode ++ vftCode
                                            ++ prefaceCode 
                                            ++ staticInitializerCode ++ initializerCode ++ methodCode
  where
    prefaceCode = ["section .text"] ++ ["__exit_portal:", "mov esp, ebp", "pop ebp", "ret"]
    staticInitLabel = "__class_" ++ (show classid) ++ "_static_initializer"
    staticInitializerCode = ["; Static initializer", "global " ++ staticInitLabel, staticInitLabel ++ ":"] ++ (concat $ map (genExprAsm sd) $ classInitializer cc) ++ ["ret"]
    classid = (typeIDMap sd) ! sym
    classIDCode = ["__classid:"] ++ ["dd " ++ (show classid)]
    externFUNC = filter (\(sym', _) -> (symbolToCN sym) /= (symbolToCN sym') || elem "native" (symbolModifiers sym')) $ toAscList (funcLabel sd)
    externSYM = toAscList (staticSYMLabelMap sd)
    stringLabels =toAscList $ stringLabel sd
    externLabels = map snd stringLabels ++ map snd (externFUNC ++ externSYM)
    externCode = ["extern " ++ (concat $ intersperse "," externLabels) ++ ", get_characteristics"]
    --fieldCode = concat $ map (genFieldAsm sd) fields
    methodCode = concat $ map (genMthdAsm sd cc) methods
    vftCode = genAsmVirtualTable sd cc
    getThis = ["mov eax, [ebp + 8]"]
    initializerCode = ["; Start a stack frame for initializer", "__initializer:", "push ebp", "mov ebp, esp"]
                      ++ getThis
                      ++ ["; put Class ID and Virtual Table"] ++ putClassIDCode ++ putvftCode
                      ++ exprs
                      ++ getThis
                      ++ initializerCodeEnding
    putClassIDCode = ["mov ebx, [__classid]", "mov [eax], ebx"]
    putvftCode = ["mov ebx, __vft", "mov [eax + 4], ebx"]
    exprs = concat $ map (genExprAsm sd) $ objectInitializer cc
    initializerCodeEnding = ["jmp __exit_portal", "; End initializer"]

genAsmVirtualTable :: SymbolDatabase -> ClassConstruct -> [String]
genAsmVirtualTable sd (CC _ _ sym _) = header ++ code
  where
    header = ["__vft:"]
    [(_, vtable)] = filter (\(symbol, _) -> symbol == sym) (instanceFUNCTable sd)
    labelT = map (\symbol -> if isNothing symbol then "__exception" else (funcLabel sd) ! (fromJust symbol)) vtable
    code = map (\str -> "dd " ++ str) labelT

--genFieldAsm :: SymbolDatabase -> FieldType -> [String]
--genFieldAsm sd (FT _ _ _ _ False) = []
--genFieldAsm sd fld@(FT name _ _ _ True) = ["; Class field: " ++ (show fld)]


genMthdAsm :: SymbolDatabase -> ClassConstruct -> MethodConstruct -> [String]
genMthdAsm sd cc (MC name symbol definition)
  | isNative = ["; native method"]
  | isConstructor = ["; constructor for class"] ++ header ++ ["; * Constructor"] ++ body ++ ["mov eax, [ebp + " ++ (show (thisOffset * 4)) ++ "]", ";default return this"] ++ ending
  | otherwise = header ++ body ++ ending
  where
    isNative = elem "native" $ symbolModifiers symbol
    isConstructor = elem "cons" $ symbolModifiers symbol
    thisOffset = 1 + 1 + (length (parameterTypes symbol))
    label = case lookup symbol (funcLabel sd) of
              Just lb -> lb
    header = ["global " ++ label, label ++ ":", "; Start a new stack frame", "push ebp", "mov ebp, esp"]
    body = concat $ map (genStmtAsm sd) definition
    ending = ["jmp __exit_portal", "; End of " ++ last name]



genStmtAsm :: SymbolDatabase -> DFStatement -> [String]
genStmtAsm sd (DFExpr expr) = genExprAsm sd expr
genStmtAsm sd (DFLocal expr) = genExprAsm sd expr ++ ["push eax ; Allocating stack space for local var"]
genStmtAsm sd (DFReturn Nothing) = ["; Void return, cleaning stack frame", "jmp __exit_portal"]
genStmtAsm sd (DFReturn (Just retVal)) = ["; Value return"] ++ genExprAsm sd retVal ++ ["; Cleaning stack frame", "jmp __exit_portal"]
genStmtAsm sd (DFBlock body nesting) =
  let bodyCode = concat $ map (genStmtAsm sd) body
      recoveryCode = recoverStackForBlock body
  in ["; new block"] ++ bodyCode ++ recoveryCode

genStmtAsm sd (DFIf cond trueBlock falseBlock nesting) =
  let condCode = genExprAsm sd cond
      trueCode = concat $ map (genStmtAsm sd) trueBlock
      falseCode = concat $ map (genStmtAsm sd) falseBlock
      falseLabel = ".falsePart_" ++ genLabel nesting
      endLabel = ".end_" ++ genLabel nesting
  in ["; If statement"] ++
     condCode ++
     ["; If statement comparison", "cmp eax, 1", "jne " ++ falseLabel ++ "; If statement true case"] ++
     trueCode ++
     ["jmp " ++ endLabel, "; If statement false case", falseLabel ++ ":"] ++
     falseCode ++
     [endLabel ++ ":"]


genStmtAsm sd (DFWhile cond body nesting) =
  let condCode = genExprAsm sd cond
      bodyCode = concat $ map (genStmtAsm sd) body
      topLabel = ".whileCond_" ++ genLabel nesting
      bottomLabel = ".whileBottom_" ++ genLabel nesting
  in [topLabel ++ ":", "; While statement"] ++
     condCode ++
     ["cmp eax, 1", "jne " ++ bottomLabel] ++
     bodyCode ++
     ["jmp " ++ topLabel] ++
     [bottomLabel ++ ":"]

genStmtAsm sd (DFFor initializer condition finalizer body nesting) =
  let initializerCode = genStmtAsm sd initializer
      conditionCode = genExprAsm sd condition
      finalizerCode = genStmtAsm sd finalizer
      bodyCode = concat $ map (genStmtAsm sd) body
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
genOpAsm "/" = ["cmp ebx, 0", "je __exception", "cdq", "idiv ebx"]
genOpAsm "%" = ["cmp ebx, 0", "je __exception", "cdq", "idiv ebx", "mov eax, edx"]
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
genOpAsm "&" = ["and eax, ebx"]
genOpAsm "|" = ["or eax, ebx"]
genOpAsm "=" = ["mov [eax], ebx", "mov eax, [eax]"]

checkNull = ["cmp eax, 0", "je __exception"]

genExprAsm :: SymbolDatabase ->  DFExpression -> [String]

genExprAsm sd (Super offset msuper) = [";call " ++ (show msuper) ++ " and then call init with this at " ++ (show offset)] ++ getThis ++ callSuperCode
                                      ++ ["; * Call Class Initializer"] ++ callInitializerCode
  where
    distance = (offset - 1) * (-4)
    getThis = ["mov eax, [ebp + " ++ (show distance) ++ "]"]
    -- eax = This address
    callInitializerCode = ["push eax", "call __initializer", "pop ebx"]
    callSuperCode = if isNothing msuper then [] else ["push eax", "call " ++ label, "pop ebx"]
    label = (funcLabel sd) ! (fromJust msuper)


genExprAsm sd (FunctionCall callee arguments) =
  --let argumentCode = ((intersperse "push eax") . concat $ map (genExprAsm sd) arguments) ++ ["push eax"] ++ ["; ARGUMENT!!! " ++ (show arguments)]
  let argumentsCode = concat $ map (\x -> (genExprAsm sd x) ++ ["push eax"]) arguments
      FUNC mds _ _ _ _ = callee
      staticFUNCMap = funcLabel sd
      staticFUNCLabel = case lookup callee staticFUNCMap of
                          Just lb -> lb
                          Nothing -> error (show callee)
      instanceFUNCMap = instanceFUNCOffsetMap sd
      instanceFUNCOffset = case lookup callee instanceFUNCMap of
                             Just offset -> offset
                             Nothing -> error (show callee)
      cleanupCode = ["add esp, " ++ (show $ 4 * (length arguments)) ++ " ; Pop arguments"]
  in ["; Function call to" ++ localName callee, "; arguments " ++ (show arguments)] ++
     argumentsCode ++
     (if elem "static" mds || elem "cons" mds
        then ["call " ++ staticFUNCLabel]
        else ["mov eax, [esp + " ++ (show $ ((length arguments) - 1) * 4) ++ "]"] ++ checkNull ++
             ["mov eax, [eax + 4]", "call [eax + " ++ show (instanceFUNCOffset * 4) ++ "] ; goto VF Table + offset = " ++ show (length (assocs instanceFUNCMap))])
             --- using __vft
     ++ cleanupCode

genExprAsm sd (ArrayAccess sym expr expri) =
  let refCode = genExprAsm sd (FunctionCall sym [expr, expri])
  in refCode ++
     ["mov eax, [eax]"]

genExprAsm sd (Unary op expr) =
  let exprCode = genExprAsm sd expr
      unaryCode = case op of
                    "-" -> ["neg eax"]
                    "!" -> ["xor eax, 1"]
  in [";Unary op: " ++ op] ++ exprCode ++ unaryCode

genExprAsm sd (Binary op exprL exprR) =
  let
      rightCode = genExprAsm sd exprR
      opCode = genOpAsm op
      leftCode = case op of
        "=" -> genExprLhsAsm sd exprL
        _ -> genExprAsm sd exprL
  in [";Binary op: " ++ op] ++
     leftCode ++
     ["push eax ; Push left value to stack"] ++
     rightCode ++
     ["mov ebx, eax", "pop eax ; Pop right value from stack"] ++
     opCode

genExprAsm sd (Attribute struct member) = (genExprLhsAsm sd (Attribute struct member)) ++ checkNull ++ ["mov eax, [eax]"]

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
genExprAsm sd (InstanceOf refsym expr) = ["; instanceOf"] ++ exprCode ++ instanceOfCode
  where
    exprCode = genExprAsm sd expr
    classid = (typeIDMap sd) ! refsym
    instanceOfCode= ["cmp eax, 0", "je short $+14", "mov eax, [eax]", "mov ebx, " ++ (show classid), "call get_characteristics"]

genExprAsm sd (Cast refType expr) = ["; Casting"] ++ exprCode ++ backupCode ++ castingCode ++ restoreCode
  where
    exprCode = genExprAsm sd expr
    backupCode = ["mov ecx, eax"]
    restoreCode = ["mov eax, ecx"]
    castingCode = case refType of
                    Left tp -> ["; Cast to a primitive type: " ++ (show tp)]
                    Right sym -> let classid = (typeIDMap sd) ! sym
                                     getcharacteristics = ["mov eax, [eax]", "mov ebx, " ++ (show classid), "call get_characteristics"] 
                                     checkException = ["cmp eax, 1", "jne __exception"]
                                 in getcharacteristics ++ checkException


genExprAsm sd expr@(ID (Right (offthis, symbol))) = ["; variable named " ++ (localName symbol) ++ " symbol: " ++ (show symbol)] ++ reduction ++ checkNull ++ getvalue
  where
    reduction = genExprLhsAsm sd expr
    getvalue = ["mov eax, [eax]"]


genExprAsm sd expr@(ID (Left offset)) = ["; ID offset " ++ (show offset)] ++ reduction ++ checkNull ++ getvalue
  where
    reduction = genExprLhsAsm sd expr
    getvalue = ["mov eax, [eax]"]

genExprAsm sd (Value AST.TypeByte value) = ["mov eax, " ++ value]
genExprAsm sd (Value AST.TypeShort value) = ["mov eax, " ++ value]
genExprAsm sd (Value AST.TypeInt value) = ["mov eax, " ++ value]
genExprAsm sd (Value AST.TypeChar value) =
  let charValue = show . ord $ (read value :: Char)
  in ["mov eax, " ++ charValue]
genExprAsm sd (Value AST.TypeBoolean "true") = ["mov eax, 1"]
genExprAsm sd (Value AST.TypeBoolean "false") = ["mov eax, 0"]
genExprAsm sd (Value AST.TypeNull _) = ["mov eax, 0"]
genExprAsm sd (Value (AST.Object (AST.Name ["java", "lang", "String"])) value) = genExprAsm sd (FunctionCall sym' $ (buildMalloc sym'):[charArray])
--["; XXX: String value: " ++ value]
  where
    db' = db sd
    stringLabelMap = stringLabel sd
    label = case lookup value stringLabelMap of
              Just lb -> lb
    [_, sym] = getSymbol db' ["joosc native", "Array", "Array"]
    charArray = (FunctionCall sym $ (buildMalloc sym):[Label label])
    [_, sym', _] = getSymbol db' ["java", "lang", "String", "String"]
genExprAsm sd (Value valuetype value) = ["; XXX: Unsupported value: " ++ value]
genExprAsm sd (Label lb) = ["mov eax, " ++ lb]
genExprAsm sd Null = ["; Null", "mov eax, 0"]
genExprAsm sd NOOP = ["; NOOP"]


genExprLhsAsm sd (ID (Left offset)) = if offset < 0
                                        then ["mov eax, ebp", "add eax, " ++ show distanceN ++ ";" ++ show offset]
                                        else ["mov eax, ebp", "sub eax, " ++ show distanceP ++ ";" ++ show offset]
  where
    distanceN = (offset - 1) * (-4)
    distanceP = (offset + 1) * 4

genExprLhsAsm sd (ID (Right (offthis, symbol))) = ["; LHS Right symbol for assignment"] ++ code
  where
    distance = (offthis - 1) * (-4)
    instanceSYMMap = instanceSYMOffsetMap sd
    staticSYMMap = staticSYMLabelMap sd
    nonstaticRes = lookup symbol instanceSYMMap
    staticRes = lookup symbol staticSYMMap
    code = case (nonstaticRes, staticRes) of
              (Nothing, Nothing) -> error $ show "ID: cannot find symbol"
              (Just offset, _) -> ["mov eax, [ebp + " ++ (show distance) ++ "]", "add eax, " ++ (show (offset * 4)) ++ ";accessing instance with offset " ++ (show offset)]
              (_, Just label) -> ["mov eax, " ++ label]

genExprLhsAsm sd (ArrayAccess sym expr expri) = genExprAsm sd (FunctionCall sym [expr, expri])
genExprLhsAsm sd (Attribute struct sym) = refCode ++ checkNull ++ ["add eax, " ++ show (instanceSYMOffset * 4)]
  where
    instanceSYMMap = instanceSYMOffsetMap sd
    instanceSYMOffset = case lookup sym instanceSYMMap of
                          Just offset -> offset
    refCode = genExprAsm sd struct

genExprLhsAsm sd expr = error (show expr)
