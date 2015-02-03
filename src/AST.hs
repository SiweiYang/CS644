module AST where

import Data.Maybe

import Lexical
import Parser
------------------------------------- Value Types
-- operator including =, +, -, *, /, %, &, &&, |, ||
-- operator special case := cast where we treat as binary

-- data Expression = Unary String Expression | Binary String Expression Expression | Value Primary


-- for . access, need to unify qualified name and field access
-- a.b() is parsed in weird way, or maybe not
-- just simplify to factors, where () is a factor as well
-- instantiation and array treat separately
-- note multi-dimensional array not supported

-- data Primary = Pri [String] | Cre | Arr


-------------------------------------- Control Structures
data Statement = LocalVar {localVar :: TypedVar, localValue :: Maybe Expression}
               | If { ifExpression :: Expression, ifBlock :: StatementBlock, elseBlock :: Maybe StatementBlock}
               | While { whileExpression :: Expression, whileBlock :: StatementBlock}
               | For { forInit :: Maybe Statement, forExpression :: Maybe Expression, forStatement :: Maybe Statement, forBlock :: StatementBlock}
               | Block StatementBlock
               | Expr Expression
               | Return (Maybe Expression)
               | Empty
               deriving (Show)

buildStatement :: AST -> Statement
buildStatement ast = case name ast of
                        "LocalVariableDeclarationStatement" -> buildStatement dec
                        "LocalVariableDeclaration"          -> LocalVar (TV tp nm) val
                        "IfThenStatement"                   -> If builtexp (buildBlock st1) Nothing
                        "IfThenElseStatement"               -> If builtexp (buildBlock st1) (Just (buildBlock st2))
                        "IfThenElseStatementNoShortIf"      -> If builtexp (buildBlock st1) (Just (buildBlock st2))
                        "WhileStatement"                    -> While builtexp (buildBlock st1)
                        "WhileStatementNoShortIf"           -> While builtexp (buildBlock st1)
                        "ForStatement"                      -> For fia exp sea (buildBlock st1)
                        "ForStatementNoShortIf"             -> For fia exp sea (buildBlock st1)
                        "Block"                             -> Block (buildBlock ast)
                        "EmptyStatement"                    -> Empty
                        "ExpressionStatement"               -> Expr (buildExp ast)
                        "ReturnStatement"                   -> Return exp
                        "StatementExpression"               -> Expr (buildExp ast)
                        "ForInit"                           -> buildStatement singleton
    where
        prods = production ast
        [singleton] = prods
        
        [dec] = filter (\ast -> name ast == "LocalVariableDeclaration") prods
        tp = buildType (head (filter (\ast -> name ast == "Type") (production ast)))
        nm = listToLexeme (filter (\ast -> name ast == "IDENTIFIER") (production ast))
        assign = filter (\ast -> name ast == "OptionalAssignment") (production ast)
        val = if (null assign) then Nothing else Just (buildExp (head assign))
        
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
        builtexp = buildExp ex
        exp = case e of
                [ex]    -> Just builtexp
                []      -> Nothing



-------------------------------------- Class Hierachy
data CompilationUnit = Comp { package :: Maybe [String], imports :: [[String]], definition :: TypeDec} deriving (Show)
data TypeDec = CLS { modifiers :: [String],
                     className :: String,
                     extends :: Maybe [String],
                     implements :: [[String]],
                     staticInit :: [StatementBlock],
                     constructors :: [Constructor],
                     fields :: [Field],
                     methods :: [Method]}
             | ITF { modifiers :: [String],
                     interfaceName :: String,
                     implements :: [[String]],
                     methods :: [Method]}
             deriving (Show)

unitName (CLS _ nm _ _ _ _ _ _) = nm
unitName (ITF _ nm _ _) = nm


buildAST :: [AST] -> CompilationUnit
buildAST prods = Comp (if length pk > 0 then Just pkg else Nothing) (if length im > 0 then imp else []) td
    where
        pk = filter (\ast -> name ast == "PackageDeclaration") prods
        pkg = nameToPackage ((production (head pk)) !! 1)
        im = filter (\ast -> name ast == "ImportDeclarations") prods
        imp = importsToPackages (head im)
        t = head (production (head (filter (\ast -> name ast == "TypeDeclaration") prods)))
        td = case name t of
                "ClassDeclaration" -> CLS md (toLexeme nm) ext ifcs (map buildBlock stcs) (map buildConstructor cons) (map buildField flds) (map buildMethod mtds)
                ---------------------- Interface to be done
                "InterfaceDeclaration" -> ITF md (toLexeme nm) exifcs (map buildMethod ifcmtds)
        ------------------ specific for class
        m = filter (\ast -> name ast == "Modifiers") (production t)
        md = if length m > 0 then reverse (toList (head m)) else []
        
        [nm] = filter (\ast -> name ast == "IDENTIFIER") (production t)
        
        ex = filter (\ast -> name ast == "Super") (production t)
        ext = if length ex > 0 then Just (nameToPackage (head (production (head ex)))) else Nothing
        
        ipl = filter (\ast -> name ast == "Interfaces") (production t)
        ifcs = map nameToPackage (concat (map (flatten "InterfaceType" ) ipl))
        
        
        
        cb = filter (\ast -> name ast == "ClassBody") (production t)
        cbds = expand (flatten "ClassBodyDeclaration" (head cb))
        
        stcs = filter (\ast -> name ast == "StaticInitializer") cbds
        
        cons = filter (\ast -> name ast == "ConstructorDeclaration") cbds
        
        mems = expand (filter (\ast -> name ast == "ClassMemberDeclaration") cbds)
        flds = filter (\ast -> name ast == "FieldDeclaration") mems
        mtds = filter (\ast -> name ast == "MethodDeclaration") mems
        ------------------ specific for interface
        exipl = filter (\ast -> name ast == "ExtendsInterfaces") (production t)
        exifcs = map nameToPackage (concat (map (flatten "InterfaceType" ) exipl))
        
        [ib] = filter (\ast -> name ast == "InterfaceBody") (production t)
        ifcmtds = flatten "InterfaceMemberDeclaration" ib

data Field = FLD { fieldModifiers :: [String], fieldVar :: TypedVar, fieldValue :: Maybe AST} deriving (Show)
buildField :: AST -> Field
buildField ast = FLD md (TV tp nm) ex
    where
        prods = production ast
        m = filter (\ast -> name ast == "Modifiers") prods
        md = if length m > 0 then reverse (toList (head m)) else []
        
        tp = buildType (head (filter (\ast -> name ast == "Type") prods))
        nm = listToLexeme (filter (\ast -> name ast == "IDENTIFIER") prods)
        
        e = filter (\ast -> name ast == "OptionalAssignment") prods
        ex = (if length e > 0 then (Just (head (production (head e)))) else Nothing)

data Method = MTD { methodModifiers :: [String], methodVar :: TypedVar, methodParameters :: [TypedVar], methodDefinition :: Maybe StatementBlock} deriving (Show)
buildMethod :: AST -> Method
buildMethod ast = MTD md (TV tp nm) (map buildTypedVar params) sb
    where
        prods = production (last (production ast))
        m = filter (\ast -> name ast == "Modifiers") prods
        md = case m of
            [a] -> reverse (toList a)
            []  -> []
        
        [t] = filter (\ast -> name ast == "Type" || name ast == "KEYWORD_VOID") prods
        tp = buildType t
        
        [dec] = filter (\ast -> name ast == "MethodDeclarator") prods
        decprods = production dec
        
        nm = toLexeme (last decprods)
        params = concat (map (flatten "FormalParameter") (filter (\ast -> name ast == "FormalParameterList") decprods))
        
        mb = filter (\ast -> name ast == "MethodBody") (production ast)
        [def] = mb
        [blk] = production def
        sb = case (mb, name blk == "Block") of
                ([], _)     -> Nothing
                (_, False)  -> Nothing
                _           -> Just (buildBlock blk)

data Constructor = Cons { constructorModifiers :: [String], constructorName :: String, constructorParameters :: [TypedVar], constructorInvocation :: Maybe AST, constructorDefinition :: Maybe StatementBlock} deriving (Show)
buildConstructor :: AST -> Constructor
buildConstructor ast = Cons md nm (map buildTypedVar params) coninvo sb
    where
        prods = production ast
        m = filter (\ast -> name ast == "Modifiers") prods
        md = case m of
            [a] -> reverse (toList a)
            []  -> []
        
        [dec] = filter (\ast -> name ast == "ConstructorDeclarator") prods
        decprods = production dec
        
        nm = toLexeme (last decprods)
        params = concat (map (flatten "FormalParameter") (filter (\ast -> name ast == "FormalParameterList") decprods))
        
        [def] = filter (\ast -> name ast == "ConstructorBody") prods
        ci = filter (\ast -> name ast == "ExplicitConstructorInvocation") (production def)
        coninvo = case ci of
            [invo]  -> Just invo
            []      -> Nothing
        bss = filter (\ast -> name ast == "BlockStatements") (production def)
        sb = case bss of
            [bs]    -> Just (buildBlock bs)
            []      -> Nothing

data StatementBlock = SB { statements :: [Statement]} deriving (Show)
buildBlock :: AST -> StatementBlock
buildBlock ast = SB (map buildStatement stmts)
    where
        stmts = concat (map (flattenL ["LocalVariableDeclarationStatement", "IfThenStatement", "IfThenElseStatement", "WhileStatement", "ForStatement", "Block", "EmptyStatement", "ExpressionStatement", "ReturnStatement"]) (production ast))

data TypedVar = TV {typeName :: Type, varName :: String} deriving (Show)
buildTypedVar :: AST -> TypedVar
buildTypedVar ast = TV tp nm
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
importsToPackages :: AST -> [[String]]
importsToPackages ast = case length prods of
                        1   -> [importToPackage id]
                        2   -> (importsToPackages re) ++ (importsToPackages id)
    where
        prods = production ast
        id = (head prods)
        re = (head (drop 1 prods))


--ClassMemberDeclaration
--StaticInitializer
--ConstructorDeclaration


data Expression = Unary { op :: String, expr :: Expression }
                | Binary { op :: String, exprL :: Expression, exprR :: Expression }
                | Attribute { struct :: Expression, mem :: String }
                | ArrayAccess { array :: Expression, index :: Expression } 
                | NewArray { arraytype :: Type, dimexprs :: Expression, dims :: Expression}
                | Dimension { left :: Expression, index :: Expression }
                | NewObject { classtype :: Type, arguments :: Expression }
                | Arguments { left :: Expression, expr :: Expression }
                | FunctionCall { func :: Expression, arguments :: Expression }
                | CastA { casttype :: Type, dims :: Expression, expr :: Expression }
                | CastB { castexpr :: Expression, expr :: Expression }
                | CastC { castname :: Name, dims :: Expression, expr :: Expression }
                | InstanceOf { reftype :: Type, expr :: Expression }
                | ID { identifier :: Name }
                | Value { valuetype :: Type, value :: String }
                | This
                | Null
                deriving (Show)

data Type = TypeByte | TypeShort | TypeInt | TypeChar | TypeBoolean | TypeString | TypeNull | TypeVoid
          | Object Name
          | Array Type
          deriving (Show)

data Name = Simple String
          | Qualified Name String
          deriving (Show)

------------------------------------------------------

buildName :: AST -> Name
buildName ast = case (name ast) of
                    "Name" -> buildName singleton
                    "SimpleName" -> buildName identifier
                    "QualifiedName" -> Qualified (buildName nm) (buildToken identifier)
                    "IDENTIFIER" -> Simple (buildToken ast)
    where
        [singleton] = production ast
        [nm] = findProd "Name" ast
        [identifier] = findProd "IDENTIFIER" ast

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

buildExp :: AST -> Expression
buildExp ast = case (name ast) of
                    "ExpressionStatement" -> buildExp statexpr
                    "StatementExpression" -> buildExp singleton
                    "AdditiveExpression" -> if (check "AdditiveExpression" ast) then (Binary (tk 1) (buildExp additive) (buildExp multiplicative)) else (buildExp multiplicative)
                    "ArgumentList" -> Arguments (if (check "ArgumentList" ast) then (buildExp args) else Null) (buildExp expr)
                    "ArrayAccess" -> ArrayAccess (if (check "PrimaryNoNewArray" ast) then (buildExp priarray) else (ID (buildName nm))) (if (check "Expression" ast) then (buildExp expr) else Null)
                    "ArrayCreationExpression" -> NewArray (buildType (if (check "PrimitiveType" ast) then pritype else classiftype)) (buildExp dimexprs) (if (check "Dims" ast) then (buildExp dims) else Null)
                    "Assignment" -> Binary (tk 1) (buildExp lhs) (buildExp expr)
                    "ConditionalExpression" -> buildExp singleton
                    "ConditionalAndExpression" -> if (check "ConditionalAndExpression" ast) then (Binary (tk 1) (buildExp condand) (buildExp equal)) else (buildExp equal)
                    "ConditionalOrExpression" -> if (check "ConditionalOrExpression" ast) then (Binary (tk 1) (buildExp condor) (buildExp condand)) else (buildExp condand)
                    "ClassInstanceCreationExpression" -> NewObject (buildType classtype) (if (check "ArgumentList" ast) then (buildExp args) else Null)
                    "Dims" -> if (check "Dims" ast) then (Dimension (buildExp dims) Null) else (Dimension Null Null)
                    "DimExpr" -> buildExp expr
                    "DimExprs" -> if (check "DimExprs" ast) then (Dimension (buildExp dimexprs) (buildExp dimexpr)) else (Dimension Null (buildExp dimexpr))
                    "EqualityExpression" -> if (check "EqualityExpression" ast) then (Binary (tk 1) (buildExp equal) (buildExp relational)) else (buildExp relational)
                    "Expression" -> buildExp singleton
                    "FieldAccess" -> Attribute (buildExp primary) (buildToken identifier)
                    "LeftHandSide" -> if (check "Name" ast) then (ID (buildName nm)) else (buildExp singleton)
                    "MethodInvocation" -> FunctionCall
                                          (if (check "Name" ast) then (ID (buildName nm)) else (buildExp fd))
                                          (if (check "ArgumentList" ast) then (buildExp args) else Null)
                    "MultiplicativeExpression" -> if (check "MultiplicativeExpression" ast) then (Binary (tk 1) (buildExp multiplicative) (buildExp unary)) else (buildExp unary)
                    "Primary" -> buildExp singleton
                    "PrimaryNoNewArray" -> if (check "Expression" ast) then (buildExp expr) else (buildExp singleton)
                    "RelationalExpression" -> if (check "ReferenceType" ast)
                                              then (InstanceOf (buildType referencetype) (buildExp relational)) else (
                                                    if (check "RelationalExpression" ast)
                                                    then (Binary (tk 1) (buildExp relational) (buildExp additive))
                                                    else (buildExp additive)
                                              )
                    "UnaryExpression" -> if (check "UnaryExpression" ast) then (Unary (tk 0) (buildExp unary)) else (buildExp unarynotpm)
                    "UnaryExpressionNotPlusMinus" -> if (check "UnaryExpression" ast) then (Unary (tk 0) (buildExp unary)) else (buildExp singleton)
                    "PostfixExpression" -> if (check "Name" ast) then (ID (buildName nm)) else (buildExp primary)
                    "CastExpression" -> if (check "UnaryExpression" ast)
                                        then (CastA (buildType pritype) (if (check "Dims" ast) then (buildExp dims) else Null) (buildExp unary)) else (
                                            if (check "Expression" ast) then (CastB (buildExp expr) (buildExp unarynotpm))
                                            else (CastC (buildName nm) (buildExp dims) (buildExp unarynotpm))
                                        )
                    "OptionalAssignment" -> buildExp expr
                    "Literal" -> Value (literalToType singleton) (tk 0)
                    "KEYWORD_THIS" -> This

    where
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

        choose = \k -> (production ast) !! k
        tk = \k -> buildToken (expandSingle (choose k))



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
        a = prod !! 0

literalToType :: AST -> Type
literalToType ast = case (name ast) of
                          "LITERAL_INT" -> TypeInt
                          "LITERAL_BOOL" -> TypeBoolean
                          "LITERAL_CHAR" -> TypeChar
                          "LITERAL_STRING" -> TypeString
                          "LITERAL_NULL" -> TypeNull


-- InterfaceType
