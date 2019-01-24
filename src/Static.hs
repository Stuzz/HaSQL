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
  } deriving (Show)

type TUp = (TableEnv -> TypeEnvironment)

type TTable = (String, M.Map String (Type, [ColumnModifier]))

type TColumn = (String, Type, [ColumnModifier])

type TExpression = TypeEnvironment -> (Expression, Type)

type TArgument = TypeEnvironment -> (Argument, Type)

type TLambda = TypeEnvironment -> (Lambda, Type)

type TStatement = TypeEnvironment -> TypeEnvironment

check :: Hasql -> TypeEnvironment
check = foldHasql checkAlgebra
    -- TODO: The last four types are not properly defined yet, maybe
  where
    checkAlgebra ::
         HasqlAlgebra TypeEnvironment TableEnv TUp TTable Column ColumnModifier Type TStatement TExpression Operation TArgument (Lambda,Type) Operator
    checkAlgebra =
      ( fHasql
      , fInit
      , fTable
      , fCol
      , fColmod
      , fType
      , fUp
      , (fStatDecl, assstat, operstat)
      , operation1
      , (exprarg, lamarg, colarg, lsarg)
      , lambda1
      , (fExprOper, fExprCond, fExprString, fExprBool, fExprInt, fExprIdent)
      , fOperator)
    fHasql tableEnv typeCheck = typeCheck tableEnv
    fInit = foldr (\(k, t) prev -> M.insert k t prev) M.empty
    fTable name columns =
      ( name
      , foldr (\(Column n t m) prev -> M.insert n (t, m) prev) M.empty columns)
    fCol name columnType modifiers
      | length modifiers == length (nub modifiers) =
        Column name columnType modifiers
      | otherwise = error "Duplicate column modifiers detected"
    fColmod = id
    fType = id
    fUp statementFunctions tableEnv =
      foldl
        (\env f -> f env)
        TypeEnvironment {table = tableEnv, var = M.empty}
        statementFunctions
    fStatDecl name typ fExpr env@TypeEnvironment {table = tEnv, var = vEnv} =
      if M.notMember name vEnv
        then let (_, exprType) = fExpr env
              in if typ == exprType
                   then TypeEnvironment {table = tEnv, var = M.insert name typ vEnv}
                   else error $
                        "Mismatched types during definition of " ++
                        name ++
                        ". Expected " ++
                        show typ ++ ", got " ++ show exprType ++ "."
        else error $ "Variable " ++ name ++ " is defined twice"
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
        (OperEquals, Just _) -> (Expr e1 OperEquals e1, TypeBool)
        (OperEquals, Nothing) -> error ("Arguments of (==) were not of equal type, namely "++ show e1type ++ ", "++ show e2type)
        (OperNotEquals, Just _) -> (Expr e1 OperNotEquals e2, TypeBool)
        (OperNotEquals, Nothing) -> error ("Arguments of (!=) were not of equal type, namely "++ show e1type ++ ", "++ show e2type)
        (OperLesserThan, Just _) -> (Expr e1 OperLesserThan e2, TypeBool)
        (OperLesserThan, Nothing) -> error ("Arguments of (<) were not of equal type, namely "++ show e1type ++ ", "++ show e2type)
        (OperLesserEquals, Just _) ->
          (Expr e2 OperLesserEquals e1, TypeBool)
        (OperLesserEquals, Nothing) ->
          error ("Arguments of (<=) were not of equal type, namely "++ show e1type ++ ", "++ show e2type)
        (OperGreaterThan, Just _) ->
          (Expr e1 OperGreaterThan e2, TypeBool)
        (OperGreaterThan, Nothing) -> error ("Arguments of (>) were not of equal type, namely "++ show e1type ++ ", "++ show e2type)
        (OperGreaterEquals, Just _) ->
          (Expr e1 OperGreaterEquals e2, TypeBool)
        (OperGreaterEquals, Nothing) ->
          error ("Arguments of (>=) were not of equal type, namely "++ show e1type ++ ", "++ show e2type)
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
    fOperator :: Operator -> Operator
    fOperator = id

    lambda1 :: Expression -> (Lambda, Type)
    lambda1 expr = (Lambda expr, TypeInt) -- int is a placeholder. Actual type of the lambda is inferred in the operation evaluation.


    exprarg :: TExpression -> TArgument
    exprarg expression env =
      let (e, t) = expression env
       in (ArgExpression e, t)

    lamarg :: (Lambda, Type) -> TArgument
    lamarg (l, t) env = (ArgLambda l, TypeInt) -- Int is only a placeholder.  Actual type of the lambda is inferred in the operation evaluation.

    colarg :: Column -> TArgument
    colarg c env = (ArgColumn c, TypeString) -- String as placeholder "type"
    lsarg :: [String] -> TArgument
    lsarg asl env = (ArgStringList asl, TypeString) -- String as placeholder "type"
    operation1 :: Operation -> Operation
    operation1 = id
    operstat :: Operation -> [TArgument] -> TStatement

    extractLambdaType :: Expression -> String -> TypeEnvironment -> (Lambda, Type)
    extractLambdaType expr i env =
      let (e, t) = eval expr i env
        in (Lambda e, t)
        
    eval :: Expression -> String -> TypeEnvironment -> (Expression, Type)
    eval (Expr e1 op e2) i env =
      fExprOper (const (eval e1 i env)) op (const (eval e2 i env)) "placeholder"
    eval (Conditional e1 e2 e3) i env =
      fExprCond
        (const (eval e1 i env))
        (const (eval e2 i env))
        (const (eval e3 i env))
        "placeholder"
    eval (ConstString s) _ _ = (ConstString s, TypeString)
    eval (ConstBool b) _ _ = (ConstBool b, TypeBool)
    eval (ConstInt i) _ _ = (ConstInt i, TypeInt)
    eval (Ident i) tableIdent env = 
      let TypeEnvironment {table = tenv, var = venv} = env in 
        case M.lookup i $ venv of
        Just c -> (Ident i, c)
        Nothing -> do
          case M.lookup i $ fetchTable tenv tableIdent of
            Just (t, mds) -> (Ident i, t)
            Nothing -> error ("Static error: column "++ i ++" does not exist in table "++tableIdent)


    --add column (NOT TESTED)
    operstat OperationAdd [a1, a2, a3] env = do
      let TypeEnvironment {table = tenv, var = venv} = env
      let tableIdent = extractString (fst (a1 env))
      let (Column n t1 mds) = extractColumn (fst (a2 env))
      let (Lambda expr) = extractLambda (fst (a3 env))
      case M.lookup tableIdent tenv of
        (Just table_env) ->
          case M.lookup n table_env of
            (Just _) ->
              error
                ("Column " ++ n ++ " does already exist in Table " ++ tableIdent)
            Nothing -> do 
              let (l,t) = extractLambdaType expr tableIdent env
              if (t == t1) then 
                let newTenv = M.insert n (t1, mds) table_env
                in (TypeEnvironment
                      { var = venv
                      , table = M.adjust (const newTenv) tableIdent tenv
                      })
              else error ("Type of lambda ("++ (show t) ++") is different from column type ("++(show t1)++")")
        Nothing -> error ("Table " ++ tableIdent ++ " does not exist")
    --split table
    operstat OperationSplit [a1, a2, a3] env =
      let TypeEnvironment {table = tenv, var = venv} = env
          tableIdent = extractString (fst (a1 env))
          newtablename = extractString (fst (a2 env))
          stringlist = extractStringList (fst (a3 env))
       in if M.member tableIdent tenv
            then if M.notMember newtablename tenv
                   then (TypeEnvironment
                           { var = venv
                           , table = let (name, (t, mds)) = getPrimary (fromJust (M.lookup tableIdent tenv)) in 
                                 (foldr
                                    (moveColumn tableIdent newtablename)
                                    (M.insert newtablename (M.insert name (t,mds) M.empty) tenv)
                                    stringlist)
                           })
                   else error
                          ("Table " ++ newtablename ++ " does already exist")
            else error ("Table " ++ tableIdent ++ " does not exist")
    -- Rename table
    operstat OperationRename [a1, a2] env = do
      let TypeEnvironment {table = tenv, var = venv} = env
      let tableIdent = extractString (fst (a1 env))
      let newName = extractString (fst (a2 env))
      case M.lookup tableIdent tenv of
        Just curtable ->
          case M.lookup newName tenv of
            Nothing ->
              TypeEnvironment
                { var = venv
                , table = M.insert newName curtable $ M.delete tableIdent tenv
                }
            Just _ ->
              error
                ("Table " ++
                 show newName ++ " does already exist in this environment")
        Nothing -> error ("Table " ++ show tableIdent ++ " does not exist")

    -- Normalize and Decouple not statically checked    
    operstat OperationNormalize [a1, a2, a3] env = do
      let tableIdent = extractString (fst (a1 env))
          newtablename = extractString (fst (a2 env))
          stringlist = extractStringList (fst (a3 env)) in env

    operstat OperationDecouple [a1, a2] env = 
      let tableIdent = extractString (fst (a1 env))
          stringlist = extractStringList (fst (a2 env)) in env
          
    operstat o _ env = error ("Incorrect numer of arguments to "++ show o)

moveColumn :: String -> String -> String -> TableEnv -> TableEnv
moveColumn tfrom tto col tenv = do
  let (Just tablefrom) = M.lookup tfrom tenv
  let (Just tableto) = M.lookup tto tenv
  case M.lookup col tablefrom of
    (Just (t, mds)) ->
      if isPrimary mds then
        error "Primary column cannot be split"
      else
        case M.lookup col tableto of
          Nothing ->
            let newEnv = M.adjust (\_ -> M.delete col tablefrom) tfrom tenv
            in M.adjust (\_ -> M.insert col (t, mds) tableto) tto newEnv
          (Just _) ->
            error ("Column " ++ col ++ " does already exist in table " ++ tto)
    Nothing -> error ("Column " ++ col ++ " does not exist in table " ++ tfrom)

isPrimary :: [ColumnModifier] -> Bool
isPrimary (m:ds) = if m == Primary then True else isPrimary ds
isPrimary [] = False

getPrimary ::  M.Map String (Type, [ColumnModifier]) -> (String, (Type, [ColumnModifier]))
getPrimary table = head (filter (\(key, (_,mds)) -> isPrimary mds) (M.toList table))

assstat :: String -> TExpression -> TStatement
assstat var expr env = do
  let TypeEnvironment {table = tenv, var = venv} = env
  case M.lookup var venv of
    (Just t1) -> do
      let (ex, t) = expr env
      if t1 == t
        then env
        else error
               ("Variable " ++
                var ++
                " is of type " ++
                show t1 ++ " but type " ++ show t ++ " was given")
    Nothing -> error ("Variable " ++ var ++ " does is not defined")

fetchTable :: TableEnv -> String -> M.Map String (Type, [ColumnModifier])
fetchTable env s = fromJust $ M.lookup s $ env
      
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
