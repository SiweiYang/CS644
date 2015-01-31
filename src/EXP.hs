module EXP where 

import Lexical
import Parser

------------------------------------- Value Types
-- operator including =, +, -, *, /, %, &, &&, |, ||
-- operator special case := cast where we treat as binary
data Expression = Unary String Expression
				| Binary String Expression Expression
				| Value Primary
				| Attribute Expression String

-- for . access, need to unify qualified name and field access
-- a.b() is parsed in weird way, or maybe not
-- just simplify to factors, where () is a factor as well
-- instantiation and array treat separately
-- note multi-dimensional array not supported
data Primary = ID String
             | Pri [String]
             | Object
             | Array

--	Expression:
--		AssignmentExpression
buildExp :: AST -> Expression
buildExp ast = if nm == "AssignmentExpression" then buildAssignExp ast else buildAssignExp (concat prod)
	where
		(AST nm prod) = ast

--	AssignmentExpression:
--		ConditionalExpression
--		Assignment
buildAssignExp :: AST -> Expression
buildAssignExp ast = if nextnm == "ConditionalExpression" then buildCondExp nextast else buildAssign nextast
	where
		(AST nm prod) = ast
		nextast = concat prod
		(AST nextnm nextprod) = nextast

--	ConditionalExpression:
--		ConditionalOrExpression
buildCondExp :: AST -> Expression
buildCondExp ast = if nm == "ConditionalOrExpression" then buildCondOrExp ast else buildCondExp (concat prod)
	where
		(AST nm prod) = ast

--	Assignment:
--		LeftHandSide OPERATOR_= AssignmentExpression
buildAssign :: AST -> Expression
buildAssign ast = Binary op lhs expr
	where
		(AST nm prod) = ast
		op = buildOperator (prod !! 1)
		lhs = buildLHS (prod !! 0)
		expr = buildExp (prod !! 2)

--	LeftHandSide:
--		Name
--		FieldAccess
--		ArrayAccess
buildLHS :: AST -> Expression
buildLHS ast = case nextnm of
		"Name" -> buildName nextast
		"FieldAccess" -> buildFieldAcc nextast
		"ArrayAccess" -> buildArrayAcc nextast
	where
		(AST nm prod) = ast
		nextast = concat prod
		(AST nextnm nextprod) = nextast


Name:
	SimpleName
	QualifiedName
buildName :: AST -> Expression
buildName ast = case nextnm of
		"SimpleName" -> buildSimpleName
		"QualifiedName" -> buildQualName
	where
		(AST nm prod) = ast
		nextast = concat prod
		(AST nextnm nextprod) = nextast


--	SimpleName:
--		IDENTIFIER
buildSimpleName :: AST -> Expression
buildSimpleName ast = buildID ((concat prod) !! 0)
	where 
		(AST nm prod) = ast


buildQualName :: AST -> Expression
buildQualName ast = Attribute nextnm id
	where
		(ASTT nm prod) = ast
		nextnm = buildName (prod !! 0)
		id = buildID (prod !! 2)


--	FieldAccess:
-- 		Primary . IDENTIFIER
buildFieldAcc :: AST -> Expression
buildFieldAcc ast = Attribute pri id
	where
		(ASTT nm prod) = ast
		nextnm = buildPrimary (prod !! 0)
		id = buildID (prod !! 2)


--	ArrayAccess:
--		Name [ Expression ]
--		PrimaryNoNewArray [ Expression ]
buildArrayAcc :: AST -> Expression
buildFieldAcc ast = 

--	Primary:
--		PrimaryNoNewArray
--		ArrayCreationExpression
buildPrimary :: AST -> Expression

--------------------------------------------------------

buildOperator :: AST -> String
buildOperator ast = case ast of 
	ASTT nm cont -> lexeme (fst cont)
	_           -> error "Operator"


buildID :: AST -> Expression
buildID ast = ID id
	where
		(ASTT nm cont) = ast
		(tk, _) = cont
		id = lexeme tk

