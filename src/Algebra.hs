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
        -- | ype
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
foldHasql (hasql1, init1, table1, col1, colmod1, typ1, up1, (declstat, assstat, operstat), operation1, (exprarg, lamarg, colarg, lsarg), lambda1, (operexpr, condexpr, string1, bool1, int1, ident1), operator1) =
  fHasql'
  where
    fHasql' (Hasql i u) = hasql1 (fInit i) (fUp u)
    fInit (Init ts) = init1 (map fTable ts)
    fTable (Table s cs) = table1 s (map fColumn cs)
    fColumn (Column s t cms) = col1 s (fType t) (map colmod1 cms)
    fType t = typ1 t
    fUp (Up ss) = up1 (map fStatement ss)
    fStatement (Declaration s t e) = declstat s t (fExpression e)
    fStatement (Assignment s e) = assstat s (fExpression e)
    fStatement (FunctionCall o as) = operstat (operation1 o) (map fArgument as)
    fArgument (ArgExpression e) = exprarg (fExpression e)
    fArgument (ArgLambda l) = lamarg (fLambda l)
    fArgument (ArgColumn c) = colarg (fColumn c)
    fArgument (ArgStringList ss) = lsarg ss
    fLambda (Lambda e) = lambda1 (fExpression e)
    fExpression (Expr e1 op e2) =
      operexpr (fExpression e1) (operator1 op) (fExpression e2)
    fExpression (Conditional e1 e2 e3) =
      condexpr (fExpression e1) (fExpression e2) (fExpression e3)
    fExpression (ConstString x) = string1 x
    fExpression (ConstBool x) = bool1 x
    fExpression (ConstInt x) = int1 x
    fExpression (Ident x) = ident1 x
    -- TODO: Add a case for the @Undefined@ expression. Or Not. I Don't care.
