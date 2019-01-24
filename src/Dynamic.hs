module Dynamic where

import Algebra
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Syntax

data IColumn = IColumn
  { nameICol :: String
  , typeICol :: Type
  , colmodICol :: [ColumnModifier]
  }

data Constant
  = BoolConst Bool
  | IntConst Int
  | StringConst String
  deriving (Show, Eq)

type IConstant = Environment -> Constant

type IArgument = Environment -> Argument

data IVar = IVar
  { nameIVar :: String
  , typeIVar :: Type
  , valIVar :: Constant
  }

-- data IArgument = IArgConst Constant | IArgLambda Lambda | IArgCol IColumn | IArgStringList [String]
type TableEnv = M.Map String (M.Map String IColumn)

type VarEnv = M.Map String IVar

data Environment = Environment
  { table :: TableEnv
  , var :: VarEnv
  }

data Code = Code
  { upgrade :: [String]
  , downgrade :: [String]
  }

instance Show Code where
  show Code {upgrade = u, downgrade = d} =
    concat
      [ "-- Upgrade \n"
      , intercalate "\n" u
      , "\n-- Downgrade \n"
      , intercalate "\n" d
      ]

type Migration = Environment -> (Code, Environment)

type TableName = String

generate :: Hasql -> Code
generate =
  foldHasql
    ( hasql1
    , init1
    , table1
    , col1
    , colmod1
    , id
    , up1
    , (declstat, assstat, operstat)
    , id
    , ( fArgExpr
      , \l env -> ArgLambda l
      , \icol env ->
          ArgColumn (Column (nameICol icol) (typeICol icol) (colmodICol icol))
      , \ls env -> ArgStringList ls)
    , Lambda
    , ( fExprOper
      , fExprCond
      , \val env -> StringConst val
      , \val env -> BoolConst val
      , \val env -> IntConst val
      , fExprIdent)
    , id)

fArgExpr :: IConstant -> IArgument
fArgExpr c env = ArgExpression $ convConstToExpr (c env)

convConstToExpr :: Constant -> Expression
convConstToExpr c =
  case c of
    BoolConst i -> ConstBool i
    IntConst i -> ConstInt i
    StringConst i -> ConstString i

fExprOper :: IConstant -> Operator -> IConstant -> IConstant
fExprOper e1 o e2 env = doOperator o expr1 expr2
  where
    expr1 = e1 env
    expr2 = e2 env


fExprCond :: IConstant -> IConstant -> IConstant -> IConstant
fExprCond b true false env =
  case b env of
    BoolConst True -> true env
    BoolConst False -> false env
    _ -> error "Static checking error: invalid argument given to condition"

fExprIdent :: String -> IConstant
fExprIdent i Environment {table = tenv, var = venv} =
  valIVar $ fromJust $ M.lookup i venv

hasql1 :: TableEnv -> Migration -> Code
hasql1 i u = fst $ u (Environment {table = i, var = M.empty})

init1 :: [(String, M.Map String IColumn)] -> TableEnv
init1 = foldr (\(k, icols) prev -> M.insert k icols prev) M.empty

table1 :: String -> [IColumn] -> (String, M.Map String IColumn)
table1 s cs = (s, foldr (\c prev -> M.insert (nameICol c) c prev) M.empty cs)

col1 :: String -> Type -> [ColumnModifier] -> IColumn
col1 s t cms = IColumn {typeICol = t, nameICol = s, colmodICol = cms}

colmod1 :: ColumnModifier -> ColumnModifier
colmod1 = id

up1 :: [Migration] -> Migration
up1 ms env = up' ms' (initial, env)
  where
    up' :: [Migration] -> (Code, Environment) -> (Code, Environment)
    up' ss acc = foldl (flip process) acc ss
    -- | The statements needed for the migrations, wrapped in a transaction.
    ms' = beginTransaction : ms ++ [commitTransaction]
    beginTransaction env' =
      (Code {upgrade = ["BEGIN;"], downgrade = ["COMMIT"]}, env')
    commitTransaction env' =
      (Code {upgrade = ["COMMIT;"], downgrade = ["BEGIN;"]}, env')
    process :: Migration -> (Code, Environment) -> (Code, Environment)
    process current (prevCode, prevEnv) =
      let (nextCode, nextEnv) = current prevEnv
       in (codeConcat nextCode prevCode, nextEnv)
    initial = Code {upgrade = [], downgrade = []}

declstat :: String -> Type -> IConstant -> Migration
declstat s t constExpr env =
  ( Code {upgrade = [], downgrade = []}
  , Environment
      { var =
          M.insert
            s
            (IVar {nameIVar = s, typeIVar = t, valIVar = constExpr env})
            (var env)
      , table = table env
      })

assstat :: String -> IConstant -> Migration
assstat s c env =
  ( Code {upgrade = [], downgrade = []}
  , Environment {var = M.update updateValue s (var env), table = table env})
  where
    updateValue IVar {nameIVar = n, typeIVar = t} =
      Just IVar {nameIVar = n, typeIVar = t, valIVar = c env}

operstat :: Operation -> [IArgument] -> Migration
operstat OperationAdd iargs env = doOperationAdd env tableName column lambda
  where
    args = map (\a -> a env) iargs
    tableName = extractString (head args)
    column = extractColumn (args !! 1)
    lambda = extractLambda (args !! 2)
operstat OperationSplit iargs env =
  doOperationSplit env tableName columnNames newTableName
  where
    args = map (\a -> a env) iargs
    tableName = extractString (head args)
    newTableName = extractString (args !! 1)
    columnNames = extractStringList (args !! 2)
operstat OperationDecouple iargs env = doOperationDecouple env tableNameOld columnNames
  where
    args = map (\a -> a env) iargs
    tableNameOld = extractString (head args)
    columnNames = extractStringList (args !! 1)
operstat OperationNormalize iargs env = doOperationNormalize env tableNameOld tableNameNew columnNames
  where
    args = map (\a -> a env) iargs
    tableNameOld = extractString (head args)
    tableNameNew = extractString (args !! 1)
    columnNames = extractStringList (args !! 2)
operstat OperationRename iargs env =
  doOperationRename env tableName newTableName
  where
    args = map (\a -> a env) iargs
    tableName = extractString (head args)
    newTableName = extractString (args !! 1)

getPK :: Environment -> String -> IColumn
getPK env i =
  snd $
  head $
  filter (\(k, v) -> Primary `elem` colmodICol v) (M.toList (fetchTable env i))

fetched :: Environment -> String -> [String] -> [(String, Type)]
fetched env i =
  map (\colString -> (colString, typeICol (fetchColumn colString)))
  where
    fetchColumn :: String -> IColumn
    fetchColumn c =
      fromMaybe
        (error "Static error: Splitting on nonexisting column.")
        (M.lookup c (fetchTable env i))

fetchTable :: Environment -> TableName -> M.Map String IColumn
fetchTable env s = fromJust $ M.lookup s $ table env

doOperationAdd ::
     Environment -> TableName -> IColumn -> Lambda -> (Code, Environment)
doOperationAdd env i c lambda =
  ( Code
      { upgrade =
          [ "ALTER TABLE " ++ i ++
                " ADD COLUMN " ++ nameICol c ++ " " ++
                typeTranslate (typeICol c) ++ ";\n",
            "UPDATE " ++ i ++ " " ++
                "SET " ++ nameICol c ++ " = " ++ translateLambda i lambda env ++ ";\n"
          ]
      , downgrade =
          [
            "ALTER TABLE " ++ i ++ " DROP COLUMN " ++ nameICol c ++ ";\n"
          ]
      },
  Environment { table = M.adjust (M.insert (nameICol c) c) i (table env), var = var env})

translateLambda :: TableName -> Lambda -> Environment -> String
translateLambda t (Lambda e) env
    = subTranslate' e env
    where
      subTranslate' (Expr e1 o e2) env =
          concat [subTranslate' e1 env, " ", operatorTranslate o, " ", subTranslate' e2 env]
      subTranslate' (Conditional e1 e2 e3) env =
          concat ["CASE WHEN ",
                subTranslate' e1 env, " THEN ", subTranslate' e2 env,
                " ELSE ", subTranslate' e3 env," END"]
      subTranslate' (ConstString s) env = constantTranslate $ StringConst s
      subTranslate' (ConstBool b) env = constantTranslate $ BoolConst b
      subTranslate' (ConstInt i) env = constantTranslate $ IntConst i
      subTranslate' (Ident s) env = case M.lookup s $ var env of
          Just c -> constantTranslate $ valIVar c
          Nothing -> if M.member s $ fetchTable env t
            then s
            else error "Static error: given column does not exist"

doOperationDecouple :: Environment -> String -> [String] -> (Code, Environment)
doOperationDecouple env i ss =
  ( Code
      { upgrade =
          [ "CREATE TABLE " ++ decoupledName ++" ( " ++
            nameICol columnPK ++ " " ++ show (typeICol columnPK) ++ " PRIMARY KEY NOT NULL,"
            ++ concatMap (\(colString, colType) -> concat [colString, " ", typeTranslate colType, ","]) (fetched env i ss)
            ++ ");"
          , "INSERT INTO " ++ decoupledName ++ " ( "
            ++ "SELECT  "
            ++ nameICol columnPK ++ ", " ++ nameInsert ", " ++ "FROM " ++ i ++ " " ++ ");"
          , "ALTER TABLE " ++ i ++ " DROP COLUMN " ++ nameInsert ", DROP COLUMN " ++ ";"
          ]
      , downgrade =
          [ "ALTER TABLE " ++ i ++ "ADD COLUMN " ++ nameTypeInsert ", ADD COLUMN " ++ ";"
          , "INSERT INTO " ++ i ++ " ( " ++ nameInsert ", " ++" )" ++ " ( "
          ++ "SELECT " ++ nameInsert ", "
          ++ "FROM " ++ decoupledName ++ " "
          ++ "WHERE " ++ i ++ "." ++ nameICol columnPK ++ " == " ++ decoupledName ++ "." ++ nameICol columnPK ++ ");"
          , "DROP TABLE " ++ decoupledName ++ ";"
          ]
      }
  , Environment { table = addAndRemove $ table env, var = var env })
  where
    columnPK :: IColumn
    columnPK = getPK env i
    nameInsert :: String -> String
    nameInsert x = intercalate x ss
    nameTypeInsert :: String -> String
    nameTypeInsert x = intercalate x (map prt (fetched env i ss))
      where
        prt :: (String, Type) -> String
        prt (s, t) = s ++ " " ++ typeTranslate t
    decoupledName :: String
    decoupledName = i ++ "_decoupled" ++ show (count 0)
    addAndRemove :: TableEnv -> TableEnv
    addAndRemove table = remove $ add table
      where
        remove :: TableEnv -> TableEnv
        remove oldtable = foldr (\c t -> M.adjust (M.delete c) i t) oldtable ss
        add :: TableEnv -> TableEnv
        add oldTable = foldr (\c t -> M.adjust (M.insert c (getCol c)) decoupledName t) (M.insert decoupledName M.empty oldTable) ss
        getCol :: String -> IColumn
        getCol c = IColumn { nameICol = c, typeICol = typeICol old, colmodICol = colmodICol old }
          where
            old = fromJust $ M.lookup c (fetchTable env i)
    count :: Int -> Int
    count x =
      case M.lookup (i ++ "_decoupled" ++ show x) (table env) of
        Just _ -> count x + 1
        Nothing -> x

doOperationNormalize ::
     Environment -> TableName -> TableName -> [String] -> (Code, Environment)
doOperationNormalize env i s ss =
  ( Code
    { upgrade = [
      -- Create new table
      "CREATE TABLE " ++ s ++ " ( "
      ++ "ID INT PRIMARY KEY NOT NULL, "
      ++ concatMap (\(colString, colType) -> concat [colString, " ", typeTranslate colType, ","]) (fetched env i ss)
      ++ " );",
      -- Insert data into new table
      "INSERT INTO " ++ s ++ " ( "
      ++ "SELECT DISTINCT " ++ nameInsert ", " ++ " "
      ++ "FROM " ++ i
      ++ " );",
      -- Create column in previous table
      "ALTER TABLE " ++ i ++ " ADD COLUMN " ++ s ++ " INTEGER",
      -- Update reference to new table (not sure if this is correct)
      "UPDATE " ++ i ++ " SET " ++ i ++ "." ++ s ++ " = " ++ s ++ ".id"
      ++ "FROM " ++ s ++ " "
      ++ "WHERE " ++ intercalate "AND " (map (\(colString, _) -> concat [i, ".", colString, " = ", s, ".", colString]) (fetched env i ss)),
      -- Add foreign key constraint
      "ALTER TABLE " ++ i ++ " ADD CONSTRAINT fk_" ++ i ++ "_" ++ s ++ " FOREIGN KEY (" ++ s ++ ") REFERENCES " ++ s ++ " (id);",
      -- Drop the columns that we exported from the old tables
      "ALTER TABLE " ++ i ++ " DROP COLUMN " ++ nameInsert ", DROP COLUMN " ++ ";"
    ]
      ,
      downgrade = [
        -- Re-add the columns
        "ALTER TABLE " ++ i ++ " ADD COLUMN " ++ nameInsert ", ADD COLUMN " ++ ";",
        -- Insert the data
        "INSERT INTO " ++ i ++ " ( "
        ++ "SELECT " ++ nameInsert ", " ++ " "
        ++ "FROM " ++ s ++ " "
        ++ "WHERE " ++ s ++ ".id = " ++ i ++ "." ++ s
        ++ ");",
        -- Drop new table & column referencing it in old table
        "DROP TABLE " ++ s ++ " CASCADE;"
      ]
    }
  , Environment { table = addAndRemove $ table env, var = var env })
  where
    nameInsert :: String -> String
    nameInsert x = intercalate x (map fst (fetched env i ss))
    addAndRemove :: TableEnv -> TableEnv
    addAndRemove table = remove $ add table
      where
        remove :: TableEnv -> TableEnv
        remove oldtable = foldr (\c t -> M.adjust (M.delete c) i t) oldtable ss
        add :: TableEnv -> TableEnv
        add oldTable = foldr (\c t -> M.adjust (M.insert c (getCol c)) s t) (M.insert s M.empty oldTable) ss
        getCol :: String -> IColumn
        getCol c = IColumn { nameICol = c, typeICol = typeICol old, colmodICol = colmodICol old }
          where
            old = fromJust $ M.lookup c (fetchTable env i)

doOperationSplit ::
     Environment -> TableName -> [String] -> TableName -> (Code, Environment)
doOperationSplit env i ss s =
  ( Code
      { upgrade =
          [ "CREATE TABLE " ++
            s ++
            " ( " ++
            nameICol columnPK ++
            " " ++
            typeTranslate (typeICol columnPK) ++
            " PRIMARY KEY NOT NULL, " ++
            nameTypeInsert ", " ++
            ");"
          , "INSERT INTO " ++
            s ++
            " ( " ++
            "SELECT " ++
            nameICol columnPK ++
            ", " ++ nameInsert ", " ++ " FROM " ++ i ++ " " ++ ");"
          , "ALTER TABLE " ++ i ++ " DROP COLUMN " ++ nameInsert ", DROP COLUMN " ++ ";"
          ]
      , downgrade =
          [ "ALTER TABLE " ++ i ++ " " ++ "ADD COLUMN " ++ nameTypeInsert ", ADD COLUMN " ++ ";"
          , "UPDATE " ++ i ++ " SET " ++ intercalate ", " (map (\(colString, _) -> concat [i, ".", colString, " = ", s, ".", colString]) (fetched env i ss)) ++
            " FROM " ++
            s ++
            " " ++
            " WHERE " ++
            i ++
            "." ++
            nameICol columnPK ++ " = " ++ s ++ "." ++ nameICol columnPK ++ ";"
          , "DROP TABLE " ++ s ++ ";"
          ]
      }
  , env)
  where
    columnPK :: IColumn
    columnPK = getPK env i
    nameInsert :: String -> String
    nameInsert x = intercalate x ss
    nameTypeInsert :: String -> String
    nameTypeInsert x = intercalate x (map prt (fetched env i ss))
      where
        prt :: (String, Type) -> String
        prt (s, t) = s ++ " " ++ typeTranslate t


doOperationRename :: Environment -> TableName -> TableName -> (Code, Environment)
doOperationRename env i s =
  ( Code
      { upgrade = ["ALTER TABLE " ++ i ++ " RENAME TO " ++ s]
      , downgrade = ["ALTER TABLE " ++ s ++ " RENAME TO " ++ i]
      }
  , newEnv)
  where
    oldTable = M.lookup i (table env)
    removedTable = M.delete i (table env)
    newEnv =
      case oldTable of
        Just o -> Environment {var = var env, table = M.insert s o removedTable}
        Nothing -> error "The given table does not exist."

extractIdent :: Argument -> Expression
extractIdent (ArgExpression i@(Ident s)) = i
extractIdent a = error ("Static error: Ident expected, " ++ show a ++ " given.")

extractString :: Argument -> String
extractString (ArgExpression (ConstString s)) = s
extractString a =
  error ("Static error: ConstString expected, " ++ show a ++ " given.")

extractIdents :: Argument -> [String]
extractIdents (ArgStringList ss) = ss
extractIdents a =
  error ("Static error: [String] expected, " ++ show a ++ " given.")

extractLambda :: Argument -> Lambda
extractLambda (ArgLambda l) = l
extractLambda a =
  error ("Static error: Lambda expected, " ++ show a ++ " given.")

extractColumn :: Argument -> IColumn
extractColumn (ArgColumn (Column s t cms))
  = IColumn { nameICol = s
    , typeICol = t
    , colmodICol = cms }
extractColumn a =
  error ("Static error: Column expected, " ++ show a ++ " given.")

extractStringList :: Argument -> [String]
extractStringList (ArgStringList ss) = ss
extractStringList a =
  error ("Static error: [String] expected, " ++ show a ++ " given.")

codeConcat :: Code -> Code -> Code
codeConcat c1 c2 =
  Code
    { upgrade = upgrade c2 ++ upgrade c1
    , downgrade = downgrade c1 ++ downgrade c2
    }

typeTranslate :: Type -> String
typeTranslate TypeBool = "boolean"
typeTranslate TypeString = "varchar"
typeTranslate TypeInt = "integer"

constantTranslate :: Constant -> String
constantTranslate (BoolConst x) = show x
constantTranslate (IntConst x) = show x
constantTranslate (StringConst x) = x

operatorTranslate :: Operator -> String
operatorTranslate OperAdd = "+"
operatorTranslate OperSubtract = "-"
operatorTranslate OperMultiply = "*"
operatorTranslate OperDivide = "/"
operatorTranslate OperConcatenate = "||"
operatorTranslate OperEquals = "="
operatorTranslate OperNotEquals = "!="
operatorTranslate OperLesserThan = "<"
operatorTranslate OperLesserEquals =  "<="
operatorTranslate OperGreaterThan =  ">"
operatorTranslate OperGreaterEquals =  ">="

doOperator :: Operator -> Constant -> Constant -> Constant
doOperator OperConcatenate (StringConst s1) (StringConst s2)
    = StringConst (s1 ++ s2)
doOperator OperEquals c1 c2 = BoolConst $ c1 == c2
doOperator OperNotEquals c1 c2 = BoolConst $ c1 /= c2
doOperator o (IntConst i1) (IntConst i2) = doOp' o i1 i2
  where
    doOp' :: Operator -> Int -> Int -> Constant
    doOp' OperAdd i1 i2 = IntConst (i1 + i2)
    doOp' OperSubtract i1 i2 = IntConst (i1 - i2)
    doOp' OperMultiply i1 i2 = IntConst (i1 * i2)
    doOp' OperDivide i1 i2 = IntConst $ quot i1 i2
    doOp' OperLesserThan i1 i2 = BoolConst (i1 < i2)
    doOp' OperLesserEquals i1 i2 = BoolConst (i1 <= i2)
    doOp' OperGreaterThan i1 i2 = BoolConst (i1 > i2)
    doOp' OperGreaterEquals i1 i2 = BoolConst (i1 >= i2)
    doOp' o c1 c2 = error $ concat ["Static error: operator ",
      show o, " not supported for ", show (IntConst c1), " and ", show (IntConst c2), "."]
doOperator o c1 c2 = error $ concat ["Static error: operator ",
    show o, " not supported for ", show c1, " and ", show c2, "."]
