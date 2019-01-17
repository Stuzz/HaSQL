{-# LANGUAGE ViewPatterns #-}

module Static where

import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe

import Algebra
import Syntax

type TableEnv = M.Map String (M.Map String (Type, [ColumnModifier]))

type VarEnv = M.Map String Type

data TypeEnvironment = TypeEnvironment
  { table :: TableEnv
  , var :: VarEnv
  }

type TTable = (String, M.Map String (Type, [ColumnModifier]))

type TColumn = (String, Type, [ColumnModifier])

type TExpression = TypeEnvironment -> (Expression, Type)

type TArgument = TypeEnvironment -> (Argument, Type)

type TLambda = TypeEnvironment -> (Lambda, Type)

type TStatement = TypeEnvironment -> (Statement, TypeEnvironment)

check :: Hasql -> TypeEnvironment
check = foldHasql checkAlgebra
    -- TODO: The last four types are not properly defined yet, maybe
  where
    checkAlgebra ::
         HasqlAlgebra TypeEnvironment TableEnv (TableEnv -> TypeEnvironment) TTable TColumn ColumnModifier Type TStatement TExpression Operation Argument Lambda Operator
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
      , (fExprOper, fExprCond, fExprString, fExprBool, fExprInt, fExprIdent)
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
    fExprOper expression1 operator expression2 env =
      case (operator, exprType) of
        (OperAdd, Just TypeInt) -> (Expr e1 OperAdd e2, TypeInt)
        (OperAdd, _) -> error "Arguments of addition were not both integers"
        (OperSubtract, Just TypeInt) -> (Expr e1 OperSubtract e2, TypeInt)
        (OperSubtract, _) ->
          error "Arguments of subtraction were not both integers"
        (OperMultiply, Just TypeInt) -> (Expr e1 OperMultiply e2, TypeInt)
        (OperMultiply, _) ->
          error "Arguments of multiplication were not both integers"
        (OperDivide, Just TypeInt) -> (Expr e1 OperDivide e2, TypeInt)
        (OperDivide, _) -> error "Arguments of division were not both integers"
        (OperConcatenate, Just TypeString) ->
          (Expr e1 OperConcatenate e2, TypeString)
          -- XXX: This should not be a probem though!
        (OperConcatenate, _) ->
          error "Arguments of concatenation were not both strings"
        (OperEquals, Just TypeBool) -> (Expr e1 OperEquals e1, TypeBool)
        (OperEquals, _) -> error "Arguments of (==) were not both booleans"
        (OperNotEquals, Just TypeBool) -> (Expr e1 OperNotEquals e2, TypeBool)
        (OperNotEquals, _) -> error "Arguments of (!=) were not both booleans"
        (OperLesserThan, Just TypeBool) -> (Expr e1 OperLesserThan e2, TypeBool)
        (OperLesserThan, _) -> error "Arguments of (<) were not both booleans"
        (OperLesserEquals, Just TypeBool) ->
          (Expr e2 OperLesserEquals e1, TypeBool)
        (OperLesserEquals, _) ->
          error "Arguments of (<=) were not both booleans"
        (OperGreaterThan, Just TypeBool) ->
          (Expr e1 OperGreaterThan e2, TypeBool)
        (OperGreaterThan, _) -> error "Arguments of (>) were not both booleans"
        (OperGreaterEquals, Just TypeBool) ->
          (Expr e1 OperGreaterEquals e2, TypeBool)
        (OperGreaterEquals, _) ->
          error "Arguments of (>=) were not both booleans"
      where
        (e1, e1type) = expression1 env
        (e2, e2type) = expression2 env
        exprType =
          if e1type == e2type
            then Just e1type
            else Nothing
    fExprCond fCondition fTrue fFalse env =
      case fCondition env of
        (c, TypeBool) ->
          let (tr, ttype) = fTrue env
              (fa, ftype) = fFalse env
           in if ftype == ttype
                then (Conditional c tr fa, ttype)
                else error "The conditional branches did not have the same type"
        _ -> error "Conditional was not a boolean"
    fExprString s env = (ConstString s, TypeString)
    fExprBool b env = (ConstBool b, TypeBool)
    fExprInt i env = (ConstInt i, TypeInt)
    fExprIdent s (var -> env) =
      case M.lookup s env of
        Just t -> (Ident s, t)
        Nothing -> error ("Variable " ++ s ++ " not defined")
    operator1 :: Operator -> Operator
    operator1 = id
    lamda1 :: TExpression -> TLambda
    lamda1 expr env =
      let (e, t) = expr env
       in (Lambda e, t)
    exprarg :: TExpression -> TArgument
    exprarg expression env =
      let (e, t) = expression env
       in (ArgExpression e, t)
    lamarg :: TLambda -> TArgument
    lamarg lambda env =
      let (l, t) = lambda env
       in (ArgLambda l, t)
    colarg :: Column -> TArgument
    colarg c env = (ArgColumn c, TypeString) -- String as placeholder "type"
    lsarg :: [String] -> TArgument
    lsarg asl env = (ArgStringList asl, TypeString) -- String as placeholder "type"
    operation1 :: Operation -> Operation
    operation1 = id
    operstat :: Operation -> [TArgument] -> TStatement
    --add column (NOT TESTED)
    operstat (OperationAdd) [a1, a2] env = do
      let TypeEnvironment {table = tenv, var = venv} = env
      let (Ident tableIdent) = extractIdent (fst (a1 env))
      let (Column n t1 mds) = extractColumn (fst (a2 env))
      case M.lookup tableIdent tenv of
        (Just table_env) -> do
          case (M.lookup n table_env) of
            (Just _) ->
              error
                ("Column " ++ n ++ " does already exist in Table " ++ tableIdent)
            Nothing -> do
              let newTenv = M.insert n (t1, mds) table_env
               in ( FunctionCall OperationAdd (map (\a -> fst (a env)) [a1, a2])
                  , TypeEnvironment
                      { var = venv
                      , table = M.adjust (\_ -> newTenv) tableIdent tenv
                      })
        Nothing -> error ("Table " ++ tableIdent ++ " does not exist")
    --split table
    operstat (OperationSplit) [a1, a2, a3] env = do
      let TypeEnvironment {table = tenv, var = venv} = env
      let (Ident tableIdent) = extractIdent (fst (a1 env))
      let newtablename = extractString (fst (a2 env))
      let stringlist = extractStringList (fst (a3 env))
      case M.lookup tableIdent tenv of
        (Just table_env) -> do
          case (M.lookup newtablename tenv) of
            Nothing -> do
              let newEnv =
                    foldr
                      (\column -> moveColumn tableIdent newtablename column)
                      tenv
                      stringlist
               in ( FunctionCall
                      OperationSplit
                      (map (\a -> fst (a env)) [a1, a2, a3])
                  , TypeEnvironment
                      { var = venv
                      , table = (M.insert newtablename M.empty newEnv)
                      })
            Just t -> error ("Table " ++ newtablename ++ " does already exist")
        Nothing -> error ("Table " ++ tableIdent ++ " does not exist")
    moveColumn :: String -> String -> String -> TableEnv -> TableEnv
    moveColumn tfrom tto col tenv = do
      let (Just tablefrom) = M.lookup tfrom tenv
      let (Just tableto) = M.lookup tto tenv
      case M.lookup col tablefrom of
        (Just (t, mds)) ->
          case M.lookup col tableto of
            Nothing -> do
              let newEnv = M.adjust (\_ -> M.delete col tablefrom) tfrom tenv
               in M.adjust (\_ -> M.insert col (t, mds) tableto) tto newEnv
            (Just _) ->
              error ("Column " ++ col ++ " does already exist in table " ++ tto)
        Nothing ->
          error ("Column " ++ col ++ " does not exist in table " ++ tfrom)

extractIdent :: Argument -> Expression
extractIdent (ArgExpression i@(Ident s)) = i
extractIdent a = error ("Ident expected, " ++ show a ++ " given.")

extractString :: Argument -> String
extractString (ArgExpression (ConstString s)) = s
extractString a = error ("ConstString expected, " ++ show a ++ " given.")

extractIdents :: Argument -> [String]
extractIdents (ArgStringList ss) = ss
extractIdents a = error ("[String] expected, " ++ show a ++ " given.")

extractLambda :: Argument -> Lambda
extractLambda (ArgLambda l) = l
extractLambda a = error ("Lambda expected, " ++ show a ++ " given.")

extractColumn :: Argument -> Column
extractColumn (ArgColumn c) = c
extractColumn a = error ("Column expected, " ++ show a ++ " given.")

extractStringList :: Argument -> [String]
extractStringList (ArgStringList ss) = ss
extractStringList a = error ("[String] expected, " ++ show a ++ " given.")
