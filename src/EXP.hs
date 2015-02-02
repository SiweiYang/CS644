module EXP where 

import Lexical
import Parser

------------------------------------- Value Types
-- operator including =, +, -, *, /, %, &, &&, |, ||
-- operator special case := cast where we treat as binary
data Expression = Unary { op :: String, expr :: Expression }
				| Binary { op :: String, exprL :: Expression, exprR :: Expression }
				| Value { expr :: Expression }
				| Attribute { struct :: Expression, field :: Expression }
				| Name { name :: Expression, identifier :: String }
				| ID { identifier :: String }
				| Array { name :: Expression, index :: Expression} 
				| Empty

-- for . access, need to unify qualified name and field access
-- a.b() is parsed in weird way, or maybe not
-- just simplify to factors, where () is a factor as well
-- instantiation and array treat separately
-- note multi-dimensional array not supported
-- data Primary = ID String
--           | Pri [String]
--           | Object
--           | Array


buildExp :: AST -> Expression
buildExp ast = case (name ast) of
					"Expression" -> buildExp singleton
					"ConditionalExpression" -> buildExp singleton
					"Assignment" -> Binary opEQ (buildExp lhs) (buildExp expr)
					"ConditionalOrExpression" -> if (null condOr) then (buildExp singleton) else (Binary (op 1) (buildExp (head condOr)) (buildExp (head condAnd)))
					"LeftHandSide" -> buildExp singleton
					"ConditionalAndExpression" -> if (null condAnd) then (buildExp singleton) else (Binary (op 1) (buildExp (head condAnd)) (buildExp (head equal)))
					"Name" -> buildExp singleton	 
					"FieldAccess" -> Attribute (buildExp (head primary)) (buildExp (head identifier))
					"ArrayAccess" -> Array (buildExp (head (if (null name) then (priarray) else name))) (buildExp (head expr))
					"SimpleName" -> buildExp (head identifier)
					"IDENTIFIER" -> buildID ast
					"QualifiedName" -> Name (buildExp (head name)) (buildExp (head identifier))
					"EqualityExpression" -> if (null equal) then (buildExp (head relational)) else (Binary (op 1) (buildExp (head equal)) (buildExp (head relational)))
					"RelationalExpression" -> if (null relational) then (buildExp (head additive)) else (if (null additive) then (Binary (op 1) (buildExp (head relational)) (buildExp (head reftype))) else (Binary (op 1) (buildExp (head relational)) (buildExp (head additive))))
					"Primary" -> buildExp singleton
					"PrimaryNoNewArray" ->
					"ReferenceType" -> 
					"ArrayCreationExpression" -> 
	where
		[singleton] = production ast

		lhs = head (findProd "LeftHandSide" ast)
		opEQ = buildOperator (head (findProd "OPERATOR_=" ast))
		expr = head (findProd "Expression" ast)

		condAnd = findProd "ConditionalAndExpression" ast
		condOr = findProd "ConditionalOrExpression" ast
		op = \k -> buildOperator expandSingle ((production ast) !! k)

		equal = findProd "EqualityExpression" ast
		name = findProd "Name" ast
		identifier = findProd "IDENTIFIER" ast

		primary = findProd "Primary" ast
		priarray = findProd "PrimaryNoNewArray" ast

		relational = findProd "RelationalExpression" ast
		additive = findProd "AdditiveExpression" ast
		reftype = findProd "ReferenceType" ast


--------------------------------------------------------

buildOperator :: AST -> String
buildOperator ast = lexeme (fst (content ast))

buildID :: AST -> Expression
buildID ast = ID (lexeme (fst (content ast)))


findProd :: String -> AST -> [AST]
findProd nm ast = filter (\ast -> (name ast) == nm) (production ast)

