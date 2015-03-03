module AST where

import Data.Maybe
import Data.List

import Util
import Lexical
import Parser

data ASTInfo = AI { fn :: String
                    , lnF, colF :: Int
                    , lnT, colT :: Int
                    } deriving (Show)

extractASTInfo :: AST -> ASTInfo
extractASTInfo (ASTT n (tk, ti)) = AI (file ti) lnf colf lnt colt
    where
        parts = splitOneOf "\n" (lexeme tk)
        lnf = (ln ti)
        colf = (col ti)
        lnt = if length parts > 1 then lnf + (length parts) - 1 else lnf
        colt = if length parts > 1 then length (last parts) else colf + (length (lexeme tk))
        
extractASTInfo ast = AI fn lnf colf lnt colt 
    where
        prods = production ast
        AI fn lnf colf _ _ = extractASTInfo (last prods)
        AI _ _ _ lnt colt = extractASTInfo (head prods)

-------------------------------------- Control Structures
data Statement = LocalVar {localVar :: TypedVar, localValue :: Expression}
               | If { ifExpression :: Expression, ifBlock :: StatementBlock, elseBlock :: Maybe StatementBlock}
               | While { whileExpression :: Expression, whileBlock :: StatementBlock}
               | For { forInit :: Maybe Statement, forExpression :: Maybe Expression, forStatement :: Maybe Statement, forBlock :: StatementBlock}
               | Block StatementBlock
               | Expr Expression
               | Return (Maybe Expression)
               | Empty
               deriving (Show)

buildStatement :: AST -> Statement
buildStatement = buildStatement' 0

buildStatement' :: Int -> AST -> Statement
buildStatement' currentDepth ast = case name ast of
                        "LocalVariableDeclarationStatement" -> buildStatement dec
                        "LocalVariableDeclaration"          -> LocalVar (TV tp nm (extractASTInfo ast)) val
                        "IfThenStatement"                   -> If builtexp (buildBlock st1) Nothing
                        "IfThenElseStatement"               -> If builtexp (buildBlock st1) (Just (buildBlock st2))
                        "IfThenElseStatementNoShortIf"      -> If builtexp (buildBlock st1) (Just (buildBlock st2))
                        "WhileStatement"                    -> While builtexp (buildBlock st1)
                        "WhileStatementNoShortIf"           -> While builtexp (buildBlock st1)
                        "ForStatement"                      -> For fia exp sea (buildBlock st1)
                        "ForStatementNoShortIf"             -> For fia exp sea (buildBlock st1)
                        "Block"                             -> Block (buildBlock ast)
                        "EmptyStatement"                    -> Empty
                        "ExpressionStatement"               -> Expr (buildNewExp ast)
                        "ReturnStatement"                   -> Return exp
                        "StatementExpression"               -> Expr (buildNewExp ast)
                        "ForInit"                           -> buildStatement singleton
    where
        buildStatement = buildStatement' (currentDepth + 1)
        buildNewExp = buildExp (currentDepth + 1)
        prods = production ast
        [singleton] = prods

        [dec] = filter (\ast -> name ast == "LocalVariableDeclaration") prods
        tp = buildType (head (filter (\ast -> name ast == "Type") (production ast)))
        nm = listToLexeme (filter (\ast -> name ast == "IDENTIFIER") (production ast))
        [assign] = filter (\ast -> name ast == "OptionalAssignment") (production ast)
        val = buildNewExp assign

        stmts = reverse (filter (\ast -> name ast == "StatementNoShortIf" || name ast == "Statement") prods)
        st1 = stmts !! 0
        st2 = stmts !! 1

        fi = filter (\ast -> name ast == "ForInit") prods
        fia = case fi of
                [fie]   -> Just (buildStatement fie)
                []      -> Nothing
        se = filter (\ast -> name ast == "StatementExpression") prods
        sea = case se of
                [see]   -> Just (buildStatement see)
                []      -> Nothing

        e = filter (\ast -> name ast == "Expression") prods
        [ex] = e
        builtexp = buildNewExp ex
        exp = case e of
                [ex]    -> Just builtexp
                []      -> Nothing



-------------------------------------- Class Hierachy
data CompilationUnit = Comp { package :: Maybe [String],
                              imports :: [[String]],
                              definition :: TypeDec,
                              cui :: CompilationUnitInfo
                              }
data CompilationUnitInfo = CompI { packageInfo :: Maybe ASTInfo,
                                   importsInfo :: [ASTInfo]
                                   } deriving (Show)

instance Show CompilationUnit where
    show (Comp pkg imps def cui) =
                                    "{\n" ++
                                    (indent 2 body) ++ "\n" ++
                                    "}\n"
        where
            body =  (if isNothing pkg then "" else ("package: " ++ (intercalate "." (fromJust pkg)) ++ "\n")) ++
                    "implements: " ++ (intercalate ", " (map (intercalate ".") imps)) ++ "\n" ++
                    (show def)
--                    ++ "\n" ++ (show cui)

data TypeDec = CLS { modifiers :: [String],
                     className :: String,
                     extends :: Maybe [String],
                     implements :: [[String]],
                     constructors :: [Constructor],
                     fields :: [Field],
                     methods :: [Method],
                     clsi :: TDInfo}
             | ITF { modifiers :: [String],
                     interfaceName :: String,
                     implements :: [[String]],
                     methods :: [Method],
                     itfi :: TDInfo}

data TDInfo = CLSI { modifiersInfo :: [ASTInfo],
                      classNameInfo :: ASTInfo,
                      extendsInfo :: Maybe ASTInfo,
                      implementsInfo :: [ASTInfo]
                    }
            | ITFI { modifiersInfo :: [ASTInfo],
                      interfaceNameInfo :: ASTInfo,
                      implementsInfo :: [ASTInfo]
                    } deriving (Show)
instance Show TypeDec where
    show (CLS mds nm ext imps cons flds mtds clsi) =
                                    "Class " ++ nm ++ "{\n" ++
                                    (indent 2 body) ++ "\n" ++
                                    "}"
--                                    ++ "\n" ++ (show clsi)
        where
            body =  "modifiers: " ++ (intercalate ", " mds) ++ "\n" ++
                    (if isNothing ext then "" else ("extends: " ++ (intercalate "." (fromJust ext)) ++ "\n")) ++
                    "implements: " ++ (intercalate ", " (map (intercalate ".") imps)) ++ "\n" ++
                    "constructors:\n" ++
                    (indent 2 (intercalate "\n" (map show cons))) ++ "\n" ++
                    "fields:\n" ++
                    (indent 2 (intercalate "\n" (map show flds))) ++ "\n" ++
                    "methods:\n" ++
                    (indent 2 (intercalate "\n" (map show mtds)))
    show (ITF mds nm imps mtds itfi) =
                                    "Interface " ++ nm ++ "{\n" ++
                                    (indent 2 body) ++ "\n" ++
                                    "}"
--                                    ++ "\n" ++ (show itfi)
        where
            body =  "modifiers: " ++ (intercalate ", " mds) ++ "\n" ++
                    "implements: " ++ (intercalate ", " (map (intercalate ".") imps)) ++ "\n" ++
                    "methods:\n" ++
                    (indent 2 (intercalate "\n" (map show mtds)))

unitName (CLS _ nm _ _ _ _ _ _) = nm
unitName (ITF _ nm _ _ _) = nm

visibleImports :: CompilationUnit -> [[String]]
visibleImports unit =
    let ownPackage = case package unit of
            Just pkgName -> [pkgName ++ ["*"]]
            Nothing -> [[]]
        importedPackages = imports unit
        javaLang = [["java","lang","*"]]
    in ownPackage ++ importedPackages ++ javaLang

buildAST :: [AST] -> CompilationUnit
buildAST prods = Comp (if length pk > 0 then Just (nameToPackage pkgn) else Nothing) (if length im > 0 then map importToPackage ims else []) td (CompI (if length pk > 0 then Just (extractASTInfo pkg) else Nothing) (if length im > 0 then map extractASTInfo ims else []))
    where
        pk = filter (\ast -> name ast == "PackageDeclaration") prods
        [pkg] = pk
        pkgn =  (production pkg) !! 1
        
        im = filter (\ast -> name ast == "ImportDeclarations") prods
        ims = reverse (flatten "ImportDeclaration" (head im))
        
        t = head (production (head (filter (\ast -> name ast == "TypeDeclaration") prods)))
        td = case name t of
                "ClassDeclaration" -> CLS (map toLexeme ms) (toLexeme nm) ext (map nameToPackage ipls) (map buildConstructor cons) (map buildField flds) (map buildMethod mtds) (CLSI (map extractASTInfo ms) (extractASTInfo nm) exti (map extractASTInfo ipls))
                ---------------------- Interface to be done
                "InterfaceDeclaration" -> ITF (map toLexeme ms) (toLexeme nm) (map nameToPackage exipls) (map buildMethod ifcmtds) (ITFI (map extractASTInfo ms) (extractASTInfo nm) (map extractASTInfo exipls))
        ------------------ specific for class
        m = filter (\ast -> name ast == "Modifiers") (production t)
        ms = case m of
          [] -> []
          _ -> reverse (flatten "ModifierKeyword" (head m))
        
        [nm] = filter (\ast -> name ast == "IDENTIFIER") (production t)
        
        ex = filter (\ast -> name ast == "Super") (production t)
        ext = if length ex > 0 then Just (nameToPackage (head (production (head ex)))) else Nothing
        exti = if length ex > 0 then Just (extractASTInfo (head (production (head ex)))) else Nothing
        
        ipl = filter (\ast -> name ast == "Interfaces") (production t)
        ipls = reverse (concat (map (flatten "InterfaceType") ipl))
        
        cb = filter (\ast -> name ast == "ClassBody") (production t)
        cbds = expand (flatten "ClassBodyDeclaration" (head cb))
        
        cons = reverse (filter (\ast -> name ast == "ConstructorDeclaration") cbds)
        
        mems = expand (filter (\ast -> name ast == "ClassMemberDeclaration") cbds)
        flds = reverse (filter (\ast -> name ast == "FieldDeclaration") mems)
        mtds = reverse (filter (\ast -> name ast == "MethodDeclaration") mems)
        ------------------ specific for interface
        exipl = filter (\ast -> name ast == "ExtendsInterfaces") (production t)
        exipls = reverse (concat (map (flatten "InterfaceType" ) exipl))
        
        [ib] = filter (\ast -> name ast == "InterfaceBody") (production t)
        ifcmtds = reverse (flatten "InterfaceMemberDeclaration" ib)

data FieldInfo = FLDI {fieldModifiersInfo :: [ASTInfo], fieldValueInfo :: Maybe ASTInfo} deriving (Show)
data Field = FLD { fieldModifiers :: [String], fieldVar :: TypedVar, fieldValue :: Maybe Expression, fldi :: FieldInfo} deriving (Show)
buildField :: AST -> Field
buildField ast = FLD (map toLexeme ms) (TV tp nm (extractASTInfo ast)) ex (FLDI (map extractASTInfo ms) exi)
    where
        prods = production ast
        m = filter (\ast -> name ast == "Modifiers") prods
        ms = case m of
            [a] -> reverse (flatten "ModifierKeyword" (head m))
            []  -> []
        
        [t] = filter (\ast -> name ast == "Type") prods
        tp = buildType t
        [n] = filter (\ast -> name ast == "IDENTIFIER") prods
        nm = toLexeme n
        
        e = filter (\ast -> name ast == "OptionalAssignment") prods
        ex = (if length e > 0 then (Just (buildExp 0 (head (production (head e))))) else Nothing)
        exi = (if length e > 0 then (Just (extractASTInfo (head (production (head e))))) else Nothing)

data MethodInfo = MTDI {methodModifiersInfo :: [ASTInfo]} deriving (Show)
data Method = MTD { methodModifiers :: [String], methodVar :: TypedVar, methodParameters :: [TypedVar], methodDefinition :: Maybe StatementBlock, mtdi :: MethodInfo} deriving (Show)
buildMethod :: AST -> Method
buildMethod ast = MTD (map toLexeme ms) (TV tp nm (extractASTInfo ast)) (map buildTypedVar params) sb (MTDI (map extractASTInfo ms))
    where
        prods = production (last (production ast))
        m = filter (\ast -> name ast == "Modifiers") prods
        ms = case m of
            [a] -> reverse (flatten "ModifierKeyword" (head m))
            []  -> []
        
        [t] = filter (\ast -> name ast == "Type" || name ast == "KEYWORD_VOID") prods
        tp = buildType t
        
        [dec] = filter (\ast -> name ast == "MethodDeclarator") prods
        decprods = production dec
        
        nm = toLexeme (last decprods)
        params = reverse $ concat (map (flatten "FormalParameter") (filter (\ast -> name ast == "FormalParameterList") decprods))
        
        mb = filter (\ast -> name ast == "MethodBody") (production ast)
        [def] = mb
        [blk] = production def
        sb = case (mb, name blk == "Block") of
                ([], _)     -> Nothing
                (_, False)  -> Nothing
                _           -> Just (buildBlock blk)

data ConstructorInfo = ConsI {constructorModifiersInfo :: [ASTInfo], constructorNameInfo :: ASTInfo, constructorInvocationInfo :: Maybe ASTInfo} deriving (Show)
data Constructor = Cons { constructorModifiers :: [String], constructorName :: String, constructorParameters :: [TypedVar], constructorInvocation :: Maybe Expression, constructorDefinition :: Maybe StatementBlock, consi :: ConstructorInfo} deriving (Show)
buildConstructor :: AST -> Constructor
buildConstructor ast = Cons (map toLexeme ms) nm (map buildTypedVar params) coninvo sb (ConsI (map extractASTInfo ms) (extractASTInfo n) coninvoi)
    where
        prods = production ast
        m = filter (\ast -> name ast == "Modifiers") prods
        ms = case m of
            [a] -> reverse (flatten "ModifierKeyword" (head m))
            []  -> []
        
        [dec] = filter (\ast -> name ast == "ConstructorDeclarator") prods
        decprods = production dec
        
        n = last decprods
        nm = toLexeme n
        params = concat (map (flatten "FormalParameter") (filter (\ast -> name ast == "FormalParameterList") decprods))
        
        [def] = filter (\ast -> name ast == "ConstructorBody") prods
        ci = filter (\ast -> name ast == "ExplicitConstructorInvocation") (production def)
        coninvo = case ci of
            [invo]  -> Just (buildExp 0 invo)
            []      -> Nothing
        coninvoi = case ci of
            [invo]  -> Just (extractASTInfo invo)
            []      -> Nothing
        bss = filter (\ast -> name ast == "BlockStatements") (production def)
        sb = case bss of
            [bs]    -> Just (buildBlock bs)
            []      -> Nothing

data StatementBlock = SB { statements :: [Statement], statementsInfo :: ASTInfo} deriving (Show)
buildBlock :: AST -> StatementBlock
buildBlock ast = SB (map buildStatement stmts) (extractASTInfo ast)
    where
        stmts = concat (map (flattenL ["LocalVariableDeclarationStatement", "IfThenStatement", "IfThenElseStatement", "WhileStatement", "ForStatement", "Block", "EmptyStatement", "ExpressionStatement", "ReturnStatement"]) (production ast))

data TypedVar = TV {typeName :: Type, varName :: String, varInfo :: ASTInfo} deriving (Show)
buildTypedVar :: AST -> TypedVar
buildTypedVar ast = TV tp nm (extractASTInfo ast)
    where
        prods = production ast
        nm = toLexeme (head prods)
        tp = buildType (last prods)

-- rethink
{-
buildType :: AST -> [String]
buildType ast = case (nast, name arr) of
                    ([], _)             -> nameToPackage ast
                    (_, "ArrayType")    -> nameToPackage ((production arr) !! 2) ++ ["[]"]
                    _                   -> nameToPackage ast
    where
        nast = flattenL ["ArrayType", "ClassOrInterfaceType", "PrimitiveType"] ast
        [arr] = nast
-}

toLexeme :: AST -> String
toLexeme ast = case ast of
                ASTT n c  -> (lexeme (fst c))
                _         -> concat (map toLexeme (production ast))

listToLexeme :: [AST] -> String
listToLexeme ast = concat (map toLexeme ast)

toList :: AST -> [String]
toList ast = case ast of
                ASTT n c  -> [toLexeme ast]
                _         -> concat (map toList prods)
    where
        prods = production ast
        id = (head prods)
        re = (head (tail prods))

--- flatten to a certain level identified by name
flatten :: String -> AST -> [AST]
flatten target ast = case ast of
                        ASTT n c  -> []
                        _         -> if name ast == target
                                        then [ast]
                                        else concat (map (flatten target) prods)
    where
        prods = production ast
--- flatten to a certain level identified by name
flattenL :: [String] -> AST -> [AST]
flattenL targets ast = case ast of
                        ASTT n c  -> []
                        _         -> if elem (name ast) targets
                                        then [ast]
                                        else concat (map (flattenL targets) prods)
    where
        prods = production ast

expand :: [AST] -> [AST]
expand asts = concat (map production asts)

nameToPackage :: AST -> [String]
nameToPackage ast = case ast of
                        ASTT n c  -> [toLexeme ast]
                        _         -> if length prods == 1
                                        then nameToPackage id
                                        else (nameToPackage re) ++ (nameToPackage id)
    where
        prods = production ast
        id = (head prods)
        re = (head (drop 2 prods))

importToPackage :: AST -> [String]
importToPackage ast = case length prods of
                        1   -> importToPackage id
                        3   -> nameToPackage st
                        5   -> (nameToPackage na) ++ (nameToPackage st)
    where
        prods = production ast
        id = (head prods)
        na = prods !! 3
        st = prods !! 1

--ClassMemberDeclaration
--StaticInitializer
--ConstructorDeclaration


data Expression = Unary { op :: String, expr :: Expression, depth :: Int}
                | Binary { op :: String, exprL :: Expression, exprR :: Expression, depth :: Int }
                | Attribute { struct :: Expression, mem :: String, depth :: Int }
                | ArrayAccess { array :: Expression, index :: Expression, depth :: Int }
                | NewArray { arraytype :: Type, dimexprs :: Expression, dims :: Expression, depth :: Int }
                | Dimension { left :: Expression, index :: Expression, depth :: Int }
                | NewObject { classtype :: Type, arguments :: Arguments, depth :: Int }
                | FunctionCall { func :: Expression, arguments :: Arguments, depth :: Int }
                | CastA { casttype :: Type, dims :: Expression, expr :: Expression, depth :: Int }
                | CastB { castexpr :: Expression, expr :: Expression, depth :: Int }
                | CastC { castname :: Name, dims :: Expression, expr :: Expression, depth :: Int }
                | InstanceOf { reftype :: Type, expr :: Expression, depth :: Int }
                | ID { identifier :: Name, depth :: Int }
                | Value { valuetype :: Type, value :: String, depth :: Int }
                | This
                | Null
                deriving (Eq, Show)

data Type = TypeByte | TypeShort | TypeInt | TypeChar | TypeBoolean | TypeString | TypeNull | TypeVoid
          | Object Name
          | Array Type
          deriving (Eq, Show)

data Name = Name [String]
          deriving (Eq, Show)

type Arguments = [Expression]

------------------------------------------------------
buildArgs :: Int -> AST -> Arguments
buildArgs currentDepth ast = map (buildExp currentDepth) lst
    where
        lst = reverse $ flatten "Expression" ast

buildName :: AST -> Name
buildName ast = case (name ast) of
                    "Name" -> buildName singleton
                    "SimpleName" -> buildName identifier
                    "QualifiedName" -> Name (parts ++ [(buildToken identifier)])
                    "IDENTIFIER" -> Name [(buildToken ast)]
    where
        [singleton] = production ast
        [nm] = findProd "Name" ast
        [identifier] = findProd "IDENTIFIER" ast
        Name parts = (buildName nm)

------------------------------------------------------

buildType :: AST -> Type
buildType ast = case (name ast) of
                    "Type" -> buildType singleton
                    "PrimitiveType" -> buildType singleton
                    "ReferenceType" -> buildType singleton
                    "ClassType" -> buildType singleton
                    "InterfaceType" -> buildType singleton
                    "KEYWORD_BYTE" -> TypeByte
                    "KEYWORD_SHORT" -> TypeShort
                    "KEYWORD_INT" -> TypeInt
                    "KEYWORD_CHAR" -> TypeChar
                    "KEYWORD_BOOLEAN" -> TypeBoolean
                    "KEYWORD_VOID" -> TypeVoid
                    "ClassOrInterfaceType" -> Object (buildName singleton)
                    "ArrayType" -> Array (if (check "PrimitiveType" ast) then (buildType pritype) else (Object (buildName nm)))
    where
        [singleton] = production ast
        [pritype] = findProd "PrimitiveType" ast
        [nm] = findProd "Name" ast

------------------------------------------------------

buildExp :: Int -> AST -> Expression
buildExp currentDepth ast = case (name ast) of
                    "ExpressionStatement" -> buildNewExp statexpr
                    "StatementExpression" -> buildNewExp singleton
                    "AdditiveExpression" -> if (check "AdditiveExpression" ast) then (Binary (tk "AdditiveOperator" ast) (buildNewExp additive) (buildNewExp multiplicative) currentDepth) else (buildNewExp multiplicative)
                    "ArrayAccess" -> ArrayAccess (if (check "PrimaryNoNewArray" ast) then (buildNewExp priarray) else (ID (buildName nm) currentDepth)) (if (check "Expression" ast) then (buildNewExp expr) else Null) currentDepth
                    "ArrayCreationExpression" -> NewArray (buildType (if (check "PrimitiveType" ast) then pritype else classiftype)) (buildNewExp dimexprs) (if (check "Dims" ast) then (buildNewExp dims) else Null) currentDepth
                    "Assignment" -> Binary "=" (buildNewExp lhs) (buildNewExp expr) currentDepth
                    "ConditionalExpression" -> buildNewExp singleton
                    "ConditionalAndExpression" -> if (check "ConditionalAndExpression" ast) then (Binary (tk "AndOperator" ast) (buildNewExp condand) (buildNewExp equal) currentDepth) else (buildNewExp equal)
                    "ConditionalOrExpression" -> if (check "ConditionalOrExpression" ast) then (Binary (tk "OrOperator" ast) (buildNewExp condor) (buildNewExp condand) currentDepth) else (buildNewExp condand)
                    "ClassInstanceCreationExpression" -> NewObject (buildType classtype) (if (check "ArgumentList" ast) then (buildNewArgs args) else []) currentDepth
                    "Dims" -> if (check "Dims" ast) then (Dimension (buildNewExp dims) Null currentDepth) else (Dimension Null Null currentDepth)
                    "DimExpr" -> buildNewExp expr
                    "DimExprs" -> if (check "DimExprs" ast) then (Dimension (buildNewExp dimexprs) (buildNewExp dimexpr) currentDepth) else (Dimension Null (buildNewExp dimexpr) currentDepth)
                    "EqualityExpression" -> if (check "EqualityExpression" ast) then (Binary "==" (buildNewExp equal) (buildNewExp relational) currentDepth) else (buildNewExp relational)
                    "Expression" -> buildNewExp singleton
                    "FieldAccess" -> Attribute (buildNewExp primary) (buildToken identifier) currentDepth
                    "LeftHandSide" -> if (check "Name" ast) then (ID (buildName nm) currentDepth) else (buildNewExp singleton)
                    "MethodInvocation" -> FunctionCall
                                          (if (check "Name" ast) then (ID (buildName nm) currentDepth) else (buildNewExp fd))
                                          (if (check "ArgumentList" ast) then (buildNewArgs args) else [])
                                          currentDepth
                    "MultiplicativeExpression" -> if (check "MultiplicativeExpression" ast) then (Binary (tk "MultiplicativeOperator" ast) (buildNewExp multiplicative) (buildNewExp unary) currentDepth) else (buildNewExp unary)
                    "Primary" -> buildNewExp singleton
                    "PrimaryNoNewArray" -> if (check "Expression" ast) then (buildNewExp expr) else (buildNewExp singleton)
                    "RelationalExpression" -> if (check "ReferenceType" ast)
                                              then (InstanceOf (buildType referencetype) (buildNewExp relational) currentDepth) else (
                                                    if (check "RelationalExpression" ast)
                                                    then (Binary (tk "CompareOperator" ast) (buildNewExp relational) (buildNewExp additive) currentDepth)
                                                    else (buildNewExp additive)
                                              )
                    "UnaryExpression" -> if (check "UnaryExpression" ast) then (Unary "-" (buildNewExp unary) currentDepth) else (buildNewExp unarynotpm)
                    "UnaryExpressionNotPlusMinus" -> if (check "UnaryExpression" ast) then (Unary "!" (buildNewExp unary) currentDepth) else (buildNewExp singleton)
                    "PostfixExpression" -> if (check "Name" ast) then (ID (buildName nm) currentDepth) else (buildNewExp primary)
                    "CastExpression" -> if (check "UnaryExpression" ast)
                                        then (CastA (buildType pritype) (if (check "Dims" ast) then (buildNewExp dims) else Null) (buildNewExp unary) currentDepth) else (
                                            if (check "Expression" ast) then (CastB (buildNewExp expr) (buildNewExp unarynotpm) currentDepth)
                                            else (CastC (buildName nm) (buildNewExp dims) (buildNewExp unarynotpm) currentDepth)
                                        )
                    "OptionalAssignment" -> buildNewExp expr
                    "ExplicitConstructorInvocation" -> FunctionCall This (if (check "ArgumentList" ast) then (buildNewArgs  args) else []) currentDepth
                    "Literal" -> Value (literalToType singleton) (buildToken (expandSingle singleton)) currentDepth
                    "KEYWORD_THIS" -> This

    where
        buildNewExp = buildExp (currentDepth + 1)
        buildNewArgs = buildArgs (currentDepth + 1)
        [singleton] = production ast

        [statexpr] = findProd "StatementExpression" ast
        [lhs] = findProd "LeftHandSide" ast
        [expr] = findProd "Expression" ast
        [nm] = findProd "Name" ast
        [additive] = findProd "AdditiveExpression" ast
        [args] = findProd "ArgumentList" ast
        [priarray] = findProd "PrimaryNoNewArray" ast
        [multiplicative] = findProd "MultiplicativeExpression" ast
        [classiftype] = findProd "ClassOrInterfaceType" ast
        [pritype] = findProd "PrimitiveType" ast
        [dimexprs] = findProd "DimExprs" ast
        [dimexpr] = findProd "DimExpr" ast
        [dims] = findProd "Dims" ast
        [condand] = findProd "ConditionalAndExpression" ast
        [condor] = findProd "ConditionalOrExpression" ast
        [equal] = findProd "EqualityExpression" ast
        [classtype] = findProd "ClassType" ast
        [relational] = findProd "RelationalExpression" ast
        [identifier] = findProd "IDENTIFIER" ast
        [primary] = findProd "Primary" ast
        [fd] = findProd "FieldAccess" ast
        [unary] = findProd "UnaryExpression" ast
        [referencetype] = findProd "ReferenceType" ast
        [unarynotpm] = findProd "UnaryExpressionNotPlusMinus" ast

        tk = \s ast -> let [a] = (findProd s ast) in (buildToken (expandSingle a))



--------------------------------------------------------

buildToken :: AST -> String
buildToken ast = lexeme (fst (content ast))
--buildToken ast = name ast

findProd :: String -> AST -> [AST]
findProd nm ast = filter (\ast -> (name ast) == nm) (production ast)

check :: String -> AST -> Bool
check nm ast = not (null (findProd nm ast))

expandSingle :: AST -> AST
expandSingle (ASTT nm cont) = (ASTT nm cont)
expandSingle (AST nm prod) = expandSingle a
    where
        [a] = prod

literalToType :: AST -> Type
literalToType ast = case (name ast) of
                          "LITERAL_INT" -> TypeInt
                          "LITERAL_BOOL" -> TypeBoolean
                          "LITERAL_CHAR" -> TypeChar
                          "LITERAL_STRING" -> TypeString
                          "LITERAL_NULL" -> TypeNull

