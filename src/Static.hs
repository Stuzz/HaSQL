module Static where

import Data.List (nub)
import qualified Data.Map.Strict as M

import Algebra
import Syntax

type TableEnv = M.Map String (M.Map String (Type, [ColumnModifier]))

type VarEnv = M.Map String Type

data TypeEnvironment = TypeEnvironment
  { table :: TableEnv
  , var :: VarEnv
  }

type TExpression = TypeEnvironment -> (Expression, Type)

check :: Hasql -> TypeEnvironment
check = foldHasql checkAlgebra
    -- TODO: The last four types are not properly defined yet, maybe
  where
    checkAlgebra ::
         HasqlAlgebra TypeEnvironment TableEnv (TableEnv -> TypeEnvironment) ( String
                                                                             , M.Map String ( Type
                                                                                            , [ColumnModifier])) ( String
                                                                                                                 , Type
                                                                                                                 , [ColumnModifier]) ColumnModifier Type (TableEnv -> VarEnv) TExpression Operation Argument Lambda Operator
    checkAlgebra =
      ( hasql
      , init
      , table
      , col
      , colmod
      , typ
      , up
      , (declstat, assstat, operstat)
      , operation1
      , (exprarg, lamarg, colarg, lsarg)
      , lambda1
      , (operexpr, condexpr, string1, bool1, int1, ident1)
      , operator1)
    hasql tableEnv typeCheck = typeCheck tableEnv
    init tables = foldr (\(k, t) prev -> M.insert k t prev) M.empty tables
    table name columns =
      (name, foldr (\(n, t, m) prev -> M.insert n (t, m) prev) M.empty columns)
    col name columnType modifiers
      | length modifiers == length (nub modifiers) =
        (name, columnType, modifiers)
      | otherwise = error "Duplicate column modifiers detected"
    colmod = id
    typ = id
    up statementFunctions tableEnv =
      TypeEnvironment
        { table = tableEnv
        , var = M.unions $ map (\f -> f tableEnv) statementFunctions
        }
    condexpr condition true false env =
      case condition env of
        (c, TypeBool) -> do
          let (tr, ttype) = true env
          let (fa, ftype) = false env
          case ftype == ttype of
            True -> (Conditional c tr fa, ttype)
            False -> error "The conditional branches did not have the same type"
            _ -> error "Conditional was not a boolean"
    string1 e env = (e, TypeString)
    bool1 e env = (e, TypeBool)
    int1 e env = (e, TypeInt)
    ident1 (Ident s) (tenv, venv) =
      case M.lookup s venv of
        Just t -> (e, t)
        Nothing -> error ("Variable " ++ s ++ " not defined")
    operexpr expression1 op expression2 env
      | op == OperAdd =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperAdd expression2
          else error "Arguments of addition where not both integers"
      | op == OperSubtract =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperSubtract expression2
          else error "Arguments of addition where not both integers"
      | op == OperMultiply =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperMultiply expression2
          else error "Arguments of addition where not both integers"
      | op == OperDivide =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperDivide expression2
          else error "Arguments of addition where not both integers"
      | op == OperConcatenate =
        if e1type == e2type && (e1type == TypeString)
          then expression1 OperConcatenate expression2
          else error "Arguments of addition where not both strings"
      | op == OperEquals =
        if e1type == e2type
          then expression1 OperEquals expression2
          else error "Arguments of addition where not both booleans"
      | op == OperNotEquals =
        if e1type == e2type
          then expression1 OperNotEquals expression2
          else error "Arguments of addition where not both booleans"
      | op == OperLesserThan =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperLesserThan expression2
          else error "Arguments of addition where not both integers"
      | op == OperLesserEquals =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperLesserEquals expression2
          else error "Arguments of addition where not both integers"
      | op == OperGreaterThan =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperGreaterThan expression2
          else error "Arguments of addition where not both integers"
      | op == OperGreaterEquals =
        if e1type == e2type && (e1type == TypeInt)
          then expression1 OperGreaterEquals expression2
          else error "Arguments of addition where not both integers"
      where
        (e1, e1type) = expression1 env
        (e2, e2type) = expression2 env
