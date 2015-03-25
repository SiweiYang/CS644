module CodeConstruct where

import Data.Either

import AST
import Environment

data ClassLayout = {

}

data MethodLayout = {
}

data DFExpression = FunctionApplication Symbol [DFExpression]
                  | Unary { op :: String, expr :: DFExpression }
                  | Binary { op :: String, exprL :: DFExpression, exprR :: DFExpression }
                  | Attribute { struct :: DFExpression, mem :: Symbol }
                  | ArrayAccess { array :: DFExpression, index :: DFExpression }
                  | NewArray { arraytype :: Type, dimexprs :: DFExpression }
                  | Dimension { left :: DFExpression, index :: DFExpression }
                  | NewObject { classtype :: Type, arguments :: [DFExpression] }
--                  | CastA { casttype :: Type, dims :: DFExpression, expr :: DFExpression }
--                  | CastB { castexpr :: DFExpression, expr :: DFExpression }
--                  | CastC { castname :: Name, dims :: DFExpression, expr :: DFExpression }
                  | InstanceOf { reftype :: Type, expr :: DFExpression }
                  | ID { identifier :: Either Int Symbol }
                  | Value { valuetype :: Type, value :: String }
                  | This
                  | Null
                  
data DFStatement = DFIf {
  condition :: DFExpression,
  ifBlock :: [DFStatement],
  elseBlock :: [DFStatement]
} | DFWhile {
  condition :: DFExpression,
  whileBlock :: [DFStatement],
} | DFFor {
  init :: DFStatement,
  condition :: DFExpression,
  fin :: DFStatement,
  forBlock :: [DFStatement]
} | DFExpr DFExpression | DFReturn (Maybe DFExpression)
