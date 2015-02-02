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
data Statement = LocalVar {localVar :: TypedVar, localValue :: Maybe AST} | If { ifExpression :: AST, ifBlock :: StatementBlock, elseBlock :: Maybe StatementBlock} | While { whileExpression :: AST, whileBlock :: StatementBlock} | For { forInit :: Maybe AST, forExpression :: Maybe AST, forStatement :: Maybe Statement, forBlock :: StatementBlock} | Block StatementBlock | Expression AST | Return (Maybe AST) | Empty deriving (Show)
buildStatement :: AST -> Statement
buildStatement ast = case name ast of
                        "LocalVariableDeclarationStatement" -> LocalVar (TV tp nm) val
                        "IfThenStatement"                   -> If ex (buildBlock st1) Nothing
                        "IfThenElseStatement"               -> If ex (buildBlock st1) (Just (buildBlock st2))
                        "IfThenElseStatementNoShortIf"      -> If ex (buildBlock st1) (Just (buildBlock st2))
                        "WhileStatement"                    -> While ex (buildBlock st1)
                        "WhileStatementNoShortIf"           -> While ex (buildBlock st1)
                        "ForStatement"                      -> For fia exp sea (buildBlock st1)
                        "ForStatementNoShortIf"             -> For fia exp sea (buildBlock st1)
                        "Block"                             -> Block (buildBlock ast)
                        "EmptyStatement"                    -> Empty
                        "ExpressionStatement"               -> Expression ast
                        "ReturnStatement"                   -> Return exp
                        "StatementExpression"               -> Expression ast
    where
        prods = production ast
        
        [dec] = filter (\ast -> name ast == "LocalVariableDeclaration") prods
        tp = buildType (head (filter (\ast -> name ast == "Type") (production dec)))
        nm = listToLexeme (filter (\ast -> name ast == "IDENTIFIER") (production dec))
        assign = filter (\ast -> name ast == "OptionalAssignment") (production dec)
        val = case assign of
                [ass]       -> Just (head (production ass))
                []          -> Nothing
        
        stmts = reverse (filter (\ast -> name ast == "StatementNoShortIf" || name ast == "Statement") prods)
        st1 = stmts !! 0
        st2 = stmts !! 1
        
        fi = filter (\ast -> name ast == "ForInit") prods
        fia = case fi of
                [fie]   -> Just fie
                []      -> Nothing
        se = filter (\ast -> name ast == "StatementExpression") prods
        sea = case se of
                [see]   -> Just (buildStatement see)
                []      -> Nothing
        
        e = filter (\ast -> name ast == "Expression") prods
        [ex] = e
        exp = case e of
                [ex]    -> Just ex
                []      -> Nothing



-------------------------------------- Class Hierachy
data CompilationUnit = Comp { package :: Maybe [String], imports :: [[String]], definition :: TypeDec} deriving (Show)
data TypeDec = CLS { modifiers :: [String], className :: String,
                    extends :: Maybe [String], implements :: [[String]],
                    staticInit :: [StatementBlock], constructors :: [Constructor], fields :: [Field], methods :: [Method]}
             | ITF { modifiers :: [String], interfaceName :: String, implements :: [[String]], methods :: [Method]} deriving (Show)
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

data TypedVar = TV {typeName :: [String], varName :: String} deriving (Show)
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

expandSingle :: AST -> [AST]
expandSingle (ASTT n c) = [ASTT n c]
expandSingle ast = case production ast of
                    [a]         -> expandSingle a
                    _           -> production ast

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

