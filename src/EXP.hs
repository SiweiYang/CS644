module EXP where 

import AST
import Lexical
import Parser

------------------------------------- Value Types
-- operator including =, +, -, *, /, %, &, &&, |, ||
-- operator special case := cast where we treat as binary
data Expression = Unary { op :: String, expr :: Expression }
				| Binary { op :: String, exprL :: Expression, exprR :: Expression }
				| Attribute { struct :: Expression, field :: Expression }
				| ID { identifier :: String }
				| ArrayAccess { array :: Expression, index :: Expression} 
				| NewArray { ty :: String, dimexprs :: Expression, dims :: Expression}
				| This
				| Dimension { left :: Expression, index :: Expression }
				| Value { valuetype :: String, value :: String }
				| NewObject { classtype :: Expression, arguments :: Expression}
				| Arguments { left :: Expression, expr :: Expression}
				| FunctionCall { func :: Expression, arguments :: Expression}
				| Cast { casttype :: Expression, dims :: Expression, expr :: Expression}
				| Empty

data Type = Byte | Short | Int | Char | Boolean
		  | Object Name
		  | Array Type

data Name = Simple String
		  | Qualified Name String

-- for . access, need to unify qualified name and field access
-- a.b() is parsed in weird way, or maybe not
-- just simplify to factors, where () is a factor as well
-- instantiation and array treat separately
-- note multi-dimensional array not supported
-- data Primary = ID String
--           | Pri [String]
--           | Object
--           | Array

------------------------------------------------------

buildName :: AST -> Name
buildName ast = case (name ast) of
					"Name" -> buildName singleton
					"SimpleName" -> buildName identifier
					"QualifiedName" -> Qualified (buildName name) (buildToken identifier)
					"IDENTIFIER" -> Simple (buildToken ast)
	where
		[singleton] = production ast
		[name] = findProd "Name" ast
		[identifier] = findProd "IDENTIFIER" ast

------------------------------------------------------

buildType :: AST -> Type
buildType ast = case (name ast) of
					"Type" -> buildType singleton
					"PrimitiveType" -> buildType singleton
					"ReferenceType" -> buildType singleton
					"ClassType" -> buildType singleton
					"InterfaceType" -> buildType singleton 
					"KEYWORD_BYTE" -> Byte
					"KEYWORD_SHORT" -> Short
					"KEYWORD_INT" -> Int
					"KEYWORD_CHAR" -> Char
					"KEYWORD_BOOLEAN" -> Boolean
					"ClassOrInterfaceType" -> Object (buildName singleton)
					"ArrayType" -> Array (if (check "PrimitiveType") then (buildType pritype) else (Object (buildName name)))
	where
		[singleton] = production ast
		[pritype] = findProd "PrimitiveType" ast
		[name] = findProd "Name" ast

------------------------------------------------------

{-
buildExp :: AST -> Expression
buildExp ast = case (name ast) of
					"AdditiveExpression" -> if (null additive) then (buildExp (head multiplicative)) else (Binary (tk 1) (buildExp (head additive)) (buildExp (head multiplicative)))
					"ArgumentList" -> Arguments (if (check "ArgumentList") then (buildExp args) else Empty) (buildExp expr)
					"ArrayAccess" -> Array (buildExp (head (if (null name) then (priarray) else name))) (buildExp expr)
					"ArrayCreationExpression" -> NewArray "" (buildExp (head dimexprs)) (if (null dims) then Empty else (buildExp (head dims))) -- bug here!
					"Assignment" -> Binary (tk 1) (buildExp lhs) (buildExp expr)
					"ConditionalExpression" -> buildExp singleton
					"ConditionalAndExpression" -> if (null condAnd) then (buildExp singleton) else (Binary (tk 1) (buildExp (head condAnd)) (buildExp (head equal)))
					"ConditionalOrExpression" -> if (null condOr) then (buildExp singleton) else (Binary (tk 1) (buildExp (head condOr)) (buildExp (head condAnd)))
					"ClassInstanceCreationExpression" -> NewObject (buildExp (head classtype)) (if (null args) then Empty else (buildExp (head args)))
					"ClassOrInterfaceType" ->  
					"Dims" -> if (null dim) then (Dimension Empty Empty) else (Dimension (buildExp (head dim)) Empty)
					"DimExpr" -> buildExp expr
					"DimExprs" -> if (null dimexprs) then (Empty Dimension (buildExp (head dimexpr))) else (Dimension (buildExp (head dimexprs)) (buildExp (head dimexpr)))
					"EqualityExpression" -> if (null equal) then (buildExp (head relational)) else (Binary (tk 1) (buildExp (head equal)) (buildExp (head relational)))
					"Expression" -> buildExp singleton
					"FieldAccess" -> Attribute (buildExp (head primary)) (buildExp (head identifier))
					"LeftHandSide" -> buildExp singleton
					"Literal" -> Value (name singleton) (tk 0)
					"MethodInvocation" -> FunctionCall (buildExp (choose 0)) (if (null args) then Empty else (buildExp (head args)))
					"MultiplicativeExpression" -> if (null multiplicative) then (buildExp singleton) else (Binary (tk 1) (buildExp (head multiplicative)) (buildExp (head unary)))
					"Primary" -> buildExp singleton
					"PrimaryNoNewArray" -> if (null expr) (buildExp singleton) then (buildExp expr)
					"RelationalExpression" -> if (null relational) then (buildExp (head additive)) else (if (null additive) then (Binary (tk 1) (buildExp (head relational)) (buildExp (head reftype))) else (Binary (tk 1) (buildExp (head relational)) (buildExp (head additive))))
					"UnaryExpression" -> if (null unary) then (buildExp singleton) else (Unary (tk 0) (buildExp (head unary)))
					"UnaryExpressionNotPlusMinus" -> if (null unary) then (buildExp singleton) else (Unary (tk 0) (buildExp (head unary))) 
					"PostfixExpression" -> buildExp singleton
					"CastExpression" -> if (null unary) then (if (null expr) then (Cast (buildExp (head name)) (buildExp (head dims)) (buildExp (head unarynotpm))) else (Cast (buildExp expr) Empty (buildExp (head unarynotpm)))) else (Cast (buildExp (head pritype)) (if (null dims) then Empty else (buildExp (head dims))) (buildExp (head unary)))
					"IDENTIFIER" -> buildToken ast
					"KEYWORD_THIS" -> This

	where
		[singleton] = production ast

		[lhs] = findProd "LeftHandSide" ast
		[expr] = findProd "Expression" ast

		condAnd = findProd "ConditionalAndExpression" ast
		condOr = findProd "ConditionalOrExpression" ast
		choose = \k -> (production ast) !! k
		tk = \k -> buildToken expandSingle (choose k)

		equal = findProd "EqualityExpression" ast
		name = findProd "Name" ast
		identifier = findProd "IDENTIFIER" ast

		primary = findProd "Primary" ast
		priarray = findProd "PrimaryNoNewArray" ast

		relational = findProd "RelationalExpression" ast
		additive = findProd "AdditiveExpression" ast
		multiplicative = findProd "MultiplicativeExpression" ast
		unary = findProd "UnaryExpression" ast
		unarynotpm = findProd "UnaryExpressionNotPlusMinus" ast


		dimexprs = findProd "DimExprs" ast
		dimexpr = findProd "DimExpr" ast
		dim = findProd "Dims" ast

		classtype = findProd "ClassType" ast
		[args] = findProd "ArgumentList" ast

-}

--------------------------------------------------------

buildToken :: AST -> String
buildToken ast = lexeme (fst (content ast))

findProd :: String -> AST -> [AST]
findProd nm ast = filter (\ast -> (name ast) == nm) (production ast)

check :: String -> AST -> Boolean
check = not . null . findProd 