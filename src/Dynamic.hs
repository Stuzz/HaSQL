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

codeConcat :: Code -> Code -> Code
codeConcat c1 c2 = Code { upgrade c1 ++ upgrade c2, downgrade c1 ++ downgrade c2 }
