module Static where

import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe

import Algebra
import Syntax
import Dynamic

type TableEnv = M.Map String (M.Map String (Type, [ColumnModifier]))

type VarEnv = M.Map String Type

data TypeEnvironment = TypeEnvironment
  { table :: TableEnv
  , var :: VarEnv
  }

type TExpression = TypeEnvironment -> (Expression, Type)

type TArgument = TypeEnvironment -> (Argument, Type)
type TLambda = TExpression -> (Lambda, Type)
type TStatement = TypeEnvironment -> (Statement, TypeEnvironment)

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
      ( fHasql
      , fInit
      , fTable
      , fCol
      , fColmod
      , fType
      , fUp
      , (declstat, assstat, operstat)
      , operation1
      , (exprarg, lamarg, colarg, lsarg)
      , lambda1
      , (operexpr, condexpr, string1, bool1, int1, ident1)
      , operator1)
    fHasql tableEnv typeCheck = typeCheck tableEnv
    fInit tables = foldr (\(k, t) prev -> M.insert k t prev) M.empty tables
    fTable name columns =
      (name, foldr (\(n, t, m) prev -> M.insert n (t, m) prev) M.empty columns)
    fCol name columnType modifiers
      | length modifiers == length (nub modifiers) =
        (name, columnType, modifiers)
      | otherwise = error "Duplicate column modifiers detected"
    fColmod = id
    fType = id
    fUp statementFunctions tableEnv =
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

    operator1 :: Operator -> Operator
    operator1 = id

    lamda1 :: TExpression -> TLambda
    lamda1 expr env -> let (e, t) = expr env in (Lambda e, t)

    exprarg :: TExpression -> TArgument
    exprarg expression env = let (e, t) = expression env in (ArgExpression e, t)
    lamarg :: TLambda -> TArgument
    lamarg lambda env = let (l, t) = lambda env in (ArgLambda l, t)
    colarg :: Column -> TArgument
    colarg c env = ArgColumn c
    lsarg :: ArgStringList :: TArgument
    lsarg asl env = ArgStringList als

    operation1 :: Operation -> Operation
    operation1 = id

    operstat :: Operation -> [TArgument] -> TStatement
    --add column (NOT TESTED)
    operstat op [a1 , a2] env = do
            let (tenv, venv) = env
            let tableIdent = extractIdent (a1 env)
            let column = extractColumn (a2 env)
            
            case M.lookup tableIdent tenv of
                (Just table_env) -> do
                    let (Column n t1 mds, t2) = column
                    case (M.lookup table_env) of
                        Nothing -> let newTenv = M.insert n (t1, mds) table_env in (FunctionCall op [tableIdent, column], newTenv)
                        Just t -> error ("Column "++n++" does already exist in Table "++table)
                Nothing -> error ("Table "++table++" does not exist") 
    --split table
    -- operstat op [a1, a2, a3] env = do
    --         let (tenv, venv) = env
    --         let tableIdent = extractIdent (a1 env)
    --         let newtablename = extractString (a2 env)
    --         let stringlist = extractStringList (a3 env)
            
    --         case M.lookup tableIdent tenv of
    --             (Just table_env) -> do
    --                 case (M.lookup newtablename tenv) of
    --                     Nothing -> do
    --                         let columnsExist = all (map (\name -> isJust(M.lookup name table_env)) stringlist)
    --                         map (\column -> moveColumn tenv tableIdent newtablename column) stringlist
                            
    --                     Just t -> error ("Table "++newtablename++" does already exist")
    --             Nothing -> error ("Table "++table++" does not exist") 

    --         where moveColumn tenv tfrom tto col = 
    --             let column = M.lookup col (M.lookup newTenv tfrom)
    --             let newTenv = M.insert col (M.lookup tenv tto) in M.delete col (M.lookup newTenv tfrom)
                
                