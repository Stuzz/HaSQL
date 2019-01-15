module Static where

import Algebra
import qualified Data.Map as M

type TableEnv = M.Map String (M.Map String Type)
type VarEnv = M.Map String Type
data TypeEnvironment = {
    table :: TableEnv,
    var :: VarEnv,
}

type TExpression = TypeEnvironment -> (Expression, Type)

check :: Hasql -> TypeEnvironment
check h = foldHasql checkAlgebra h
    where
        checkAlgebra
        = (hasql1, init1, table1,
            col1, colmod1, typ1, up1,
            (declstat, assstat, operstat),
            operation1,
            (exprarg, lamarg, colarg, lsarg),
            lambda1,
            (operexpr, condexpr, string1, bool1, int1, ident1),
            operator1)
        hasql1 :: TableEnv -> (TableEnv -> TypeEnvironment) -> TypeEnvironment

        -- Table Environment
        init1 :: [(String, M.Map String Type)] -> TableEnv
        init1 ts = foldr (\(k, t) prev -> M.insert k t prev) M.empty ts
        table1 :: String -> [(String, Type)] -> (String, M.Map String Type)
        table1 s cs  = (s, foldr (\(k, t) prev -> M.insert k t prev) M.empty cs)
        col1 :: Column -> (String, Type)
        col1 (Column s t cms) | length cms == length unique cms = (s, t)
        col1 (Column s t cms) | otherwise = error "Duplicate column modifiers detected"
        colmod1 = id
        typ1 = id

        -- Check Up
        -- up1 :: [Statement -> TableEnv -> VarEnv] -> TableEnv -> TypeEnvironment
        -- up1

        -- operexpr :: Expression -> TypeEnvironment -> (Expression, Type)
        condexpr :: TExpression -> TExpression -> TExpression -> TExpression
        condexpr condition true false env
            = case condition env of
                (c, TypeBool) -> do
                    let (tr, ttype) = true env
                    let (fa, ftype) = false env
                    case (ftype == ttype) of
                        True -> (Conditional c tr fa, ttype)
                        False -> error "The conditional branches did not have the same type"
                otherwise -> error "Conditional was not a boolean"
        string1 :: Expression -> TExpression
        string1 e env = (e, TypeString)
        bool1 :: Expression -> TExpression
        bool1 e env = (e, TypeBool)
        int1 :: Expression -> TExpression
        int1 e env = (e, TypeInt)
        ident1 :: Expression -> TExpression
        ident1 (Ident s) (tenv, venv)
            = case M.lookup s venv of
                Just t -> (e, t)
                Nothing -> error ("Variable " ++ s ++ " not defined" )