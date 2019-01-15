module Algebra where

import Syntax

type HasqlAlgebra hasql init up table column colmod typ statement expression operation argument lambda operator
        -- | Hasql
   = ( init -> up -> hasql
        -- | Init
     , [table] -> init
        -- | Table
     , String -> [column] -> table
        -- | Column
     , String -> typ -> [colmod] -> column
        -- | ColumnModifier
     , ColumnModifier -> colmod
        -- | Type
     , Type -> typ
        -- | Up
     , [statement] -> up
        -- | Statement
     , ( String -> Type -> expression -> statement -- ^ Declaration
       , String -> expression -> statement -- ^ Assignment
       , operation -> [argument] -> statement -- ^ Function call
        )
        -- | Operation
     , Operation -> operation
        -- | Argument
     , ( expression -> argument
       , lambda -> argument
       , column -> argument
       , [String] -> argument)
        -- | Lambda
     , expression -> lambda
        -- | Expression
     , ( expression -> operator -> expression -> expression -- ^ Expression
       , expression -> expression -> expression -> expression -- ^ Conditional
       , String -> expression -- ^ ConstString
       , Bool -> expression -- ^ ConstBool
       , Int -> expression -- ^ ConstInt
       , String -> expression -- ^ Ident
        )
        -- | Operator
     , Operator -> operator)

foldHasql ::
     HasqlAlgebra hasql init up table column colmod typ statement expression operation argument lambda operator
  -> Hasql
  -> hasql
foldHasql (fHasql, fInit, fTable, fCol, fColmod, fTyp, fUp, (fStatDecl, fStatAss, fStatOper), fOperation, (fArgExpr, fArgLam, fArgCol, fArgLis), fLambda, (fExprOper, fExprCond, fExprString, fExprBool, fExprInt, fExprIdent), fExprOper) =
  fHasql'
  where
    fHasql' (Hasql i u) = fHasql (fInit' i) (fUp' u)
    fInit' (Init ts) = fInit (map fTable' ts)
    fTable' (Table s cs) = fTable s (map fColumn' cs)
    fColumn' (Column s t cms) = fCol s (fType' t) (map fColmod cms)
    fType' = fTyp
    fUp' (Up ss) = fUp (map fStatement' ss)
    fStatement' (Declaration s t e) = fStatDecl s t (fExpression' e)
    fStatement' (Assignment s e) = fStatAss s (fExpression' e)
    fStatement' (FunctionCall o as) =
      fStatOper (fOperation o) (map fArgument' as)
    fArgument' (ArgExpression e) = fArgExpr (fExpression' e)
    fArgument' (ArgLambda l) = fArgLam (fLambda' l)
    fArgument' (ArgColumn c) = fArgCol (fColumn' c)
    fArgument' (ArgStringList ss) = fArgLis ss
    fLambda' (Lambda e) = fLambda (fExpression' e)
    fExpression' (Expr e1 op e2) =
      fExprOper (fExpression' e1) (fExprOper op) (fExpression' e2)
    fExpression' (Conditional e1 e2 e3) =
      fExprCond (fExpression' e1) (fExpression' e2) (fExpression' e3)
    fExpression' (ConstString x) = fExprString x
    fExpression' (ConstBool x) = fExprBool x
    fExpression' (ConstInt x) = fExprInt x
    fExpression' (Ident x) = fExprIdent x
    -- TODO: Add a case for the @Undefined@ expression. Or Not. I Don't care.
