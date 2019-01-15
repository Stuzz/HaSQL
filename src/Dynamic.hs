module Dynamic where

import Algebra
import qualified Data.Map as M

data IColumn = IColumn {
    nameICol :: String,
    typeICol :: Type,
    colmodICol :: [ColumnModifier]
}

data Constant = BoolConst Bool
            | IntConst Int
            | StringConst String

data IVar = IVar {
    nameIVar :: String,
    typeIVar :: Type,
    valIVar :: Constant
}


type TableEnv = M.Map String (M.Map String IColumn)

type VarEnv = M.Map String IVar

data Environment = Environment {
    table :: TableEnv,
    var :: VarEnv,
}

data Code = Code {
    upgrade :: [String]
    downgrade :: [String]
}

type Migration = Environment -> (Code, Environment)

generate :: Hasql -> [String]
generate h = foldHasql generateAlgebra h
    where
        generateAlgebra =
            (hasql1, init1, table1,
            col1, colmod1, typ1, up1,
            (declstat, assstat, operstat),
            operation1,
            (exprarg, lamarg, colarg, lsarg),
            lambda1,
            (operexpr, condexpr, string1, bool1, int1, ident1),
            operator1)

hasql1 :: TableEnv -> Migration -> Code
hasql1 i u = u (Environment { table = i, var = M.empty })

init1 :: [(String, Map String IColumn)] -> TableEnv
init1 ts = foldr (\(k, icols) prev -> M.insert k icols prev) M.empty ts

table1 :: String -> [IColumn] -> (String, Map String IColumn)
table1 s cs = (s, foldr (\c prev -> M.insert (nameICol c) c prev) M.empty cs)

col1 :: String -> Type -> [ColumnModifier] -> IColumn
col1 s t cms = IColumn { typeICol = t, nameICol = s, colmodICol = cms }

colmod1 :: ColumnModifier -> ColumnModifier
colmod1 = id

up1 :: [Migration] -> Migration
up1 ms env
    = up' ms (initial, env)
    where
        up' :: [Migration] -> (Code, Environment) -> (Code, Environment)
        up' [] acc = acc
        up' (s:ss) acc = up' ss (process (s env) acc)
        process :: Migration -> (Code, Environment) -> (Code, Environment)
        process current (prevCode, prevEnv)
            = let (nextCode, nextEnv) = current prevEnv
                in (codeConcat nextCode prevCode, nextEnv)
        initial = Code {upgrade = [], downgrade = []}

declstat :: String -> Type -> Constant -> Migration
declstat s t constExpr env
    = ([], M.insert s
        (IVar {nameIVar = s, typeIVar = t, valIVar = constExpr}) env)

assstat :: String -> Constant -> Migration
assstat s c env
    = ([], M.update updateValue s env)
    where
        updateValue (IVar { nameIVar n, typeIVar t })
            = Just $ IVar { nameIVar n, typeIVar t, valueIVar c }

operstat :: Operation -> [Argument] -> Migration
operstat OperationAdd args env = doOperationAdd tableName columnName lambda
    where
        tableName = extractIdent (args!!0)
        columnName = extractColumn (args!!1)
        lambda = extractLambda (args!!2)
operstat OperationSplit args env = doOperationSplit tableName columnNames newTableName
    where
        tableName = extractIdent (args!!0)
        columnNames = extractStringList (args!!1)
        newTableName = extractString (args!!2)
operstat OperationDecouple args env = undefined
operstat OperationNormalize args env = undefined
operstat OperationRename args env = doOperationRename tableName columnNames newTableName
    where
        tableName = extractIdent (args!!0)
        columnNames = extractStringList (args!!1)
        newTableName = extractString (args!!2)

doOperationAdd :: Expression -> Column -> Lambda -> (Code, Environment)
doOperationAdd = undefined

doOperationSplit :: Expression -> [String] -> String -> (Code, Environment)
doOperationSplit = undefined

doOperationDecouple :: Expression -> [String] -> (Code, Environment)
doOperationDecouple = undefined

doOperationNormalize :: Expression -> String -> [String] -> (Code, Environment)
doOperationNormalize = undefined

doOperationRename :: Expression -> String -> (Code, Environment)
doOperationRename = undefined

extractIdent :: Argument -> Ident
extractIdent (ArgExpression i@(Ident s)) = i
extractIdent a = error "Ident expected, " ++ show a ++ " given."

extractString :: Argument -> String
extractString (ArgExpression (ConstString s)) = s
extractString a = error "ConstString expected, " ++ show a ++ " given."

extractIdents :: Argument -> [String]
extractIdents (ArgStringList ss) = ss
extractIdents a = error "[String] expected, " ++ show a ++ " given."

extractLambda :: Argument -> Lambda
extractLambda (ArgLambda l) = l
extractLambda a = error "Lambda expected, " ++ show a ++ " given."

extractColumn :: Argument -> Column
extractColumn (ArgColumn c) = c
extractColumn a = error "Column expected, " ++ show a ++ " given."

extractStringList :: Argument -> [String]
extractStringList (ArgStringList ss) = ss
extractStringList a = error "[String] expected, " ++ show a ++ " given."

codeConcat :: Code -> Code -> Code
codeConcat c1 c2 = Code { upgrade c1 ++ upgrade c2, downgrade c1 ++ downgrade c2 }
