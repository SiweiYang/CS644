module AST where

import Data.Maybe

import Lexical
import Parser
------------------------------------- Value Types
-- operator including =, +, -, *, /, %, &, &&, |, ||
-- operator special case := cast where we treat as binary
data Expression = Unary String Expression | Binary String Expression Expression | Value Primary

-- for . access, need to unify qualified name and field access
-- a.b() is parsed in weird way, or maybe not
-- just simplify to factors, where () is a factor as well
-- instantiation and array treat separately
-- note multi-dimensional array not supported
data Primary = Pri [String] | Cre | Arr


-------------------------------------- Control Structures
data Statement = STMT | If | For | While | Block | Return


-------------------------------------- Class Hierachy
data CompilationUnit = Comp { package :: Maybe [String], imports :: [[String]], definition :: TypeDec} deriving (Show)
data TypeDec = CLS { modifiers :: [String], typeName :: String, staticInit :: Maybe AST, constructor :: [AST], fields :: [AST], methods :: [AST]}
                | ITF { modifiers :: [String], interfaceName :: String, staticInit :: Maybe AST, constructors :: [AST], members :: [AST]} deriving (Show)

buildAST :: [AST] -> CompilationUnit
buildAST prods = Comp (if length pk > 0 then Just pkg else Nothing) (if length im > 0 then imp else []) td
    where
        pk = filter (\ast -> name ast == "PackageDeclaration") prods
        pkg = nameToPackage ((production (head pk)) !! 1)
        im = filter (\ast -> name ast == "ImportDeclarations") prods
        imp = importsToPackages (head im)
        t = head (production (head (filter (\ast -> name ast == "TypeDeclaration") prods)))
        td = case name t of
                "ClassDeclaration" -> CLS (if length m > 0 then md else []) nm Nothing cons flds mtds
                ---------------------- Interface to be done
        ------------------ specific for class
        m = filter (\ast -> name ast == "Modifiers") (production t)
        md = reverse (toList (head m))
        nm = toLexeme (head (filter (\ast -> name ast == "IDENTIFIER") (production t)))
        cb = filter (\ast -> name ast == "ClassBody") (production t)
        cbds = expand (flatten "ClassBodyDeclaration" (head cb))
        
        cons = filter (\ast -> name ast == "ConstructorDeclaration") cbds
        mems = expand (filter (\ast -> name ast == "ClassMemberDeclaration") cbds)
        flds = filter (\ast -> name ast == "FieldDeclaration") mems
        mtds = filter (\ast -> name ast == "MethodDeclaration") mems
        ------------------ specific for interface

data Field = FLD { fieldModifiers :: [String], fieldType :: String, fieldName :: String, fieldValue :: Maybe AST}
buildField :: AST -> Field
buildField ast = FLD md [] nm if length e > 0 then (Just (head e)) else Nothing
    where
        m = filter (\ast -> name ast == "Modifiers") (production ast)
        md = reverse (toList (head m))
        nm = listToLexeme (filter (\ast -> name ast == "IDENTIFIER") (production ast))
        
        e = filter (\ast -> name ast == "Expression") (production ast)
        

toLexeme :: AST -> String
toLexeme ast = case ast of
                ASTT n c  -> (lexeme (fst c))
                _         -> concat (map toLexeme (production ast))

listToLexeme :: [AST] -> String
listToLexeme ast = concat (map toLexeme (production ast))
                
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

