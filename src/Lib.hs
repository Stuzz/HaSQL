module Lib
  ( parseSql
  ) where

import Debug.Trace

import Algebra
import qualified Dynamic

-- import Lexer
-- import Parser
import qualified Static
import Syntax

-- | Transform a migration file into a list of SQL statements.
--
-- TODO: This should of course return SQL files for both the up and the down
--       migration.
parseSql :: String -> Dynamic.Code
parseSql = const (compile example)

-- XXX: Dangerous, don't touch, might explode
compile :: Hasql -> Dynamic.Code
compile hasql =
  let bigChungus = traceShowId $ Static.check hasql
   in const (Dynamic.generate hasql) $! bigChungus

execDyn :: Hasql -> Dynamic.Code
execDyn hasql = Dynamic.generate hasql

example :: Hasql
example = Hasql init up
  where
    init =
      Init
        [ Table
            "users"
            [ Column "id" TypeInt [Primary]
            , Column "first_name" TypeString []
            , Column "age" TypeInt []
            ]
        ]
    up =
      Up
        [ FunctionCall
            OperationSplit
            [ ArgExpression (ConstString "users")
            , ArgExpression (ConstString "names")
            , ArgStringList ["first_name"]
            ]
        ]

exampleAdd :: Hasql
exampleAdd = Hasql init up
  where
    init =
      Init
        [ Table
            "users"
            [ Column "id" TypeInt [Primary]
            , Column "first_name" TypeString []
            , Column "age" TypeInt []
            ]
        ]
    up =
      Up
        [ Declaration "adult_age" TypeInt (ConstInt 18)
        , FunctionCall
            OperationAdd
            [ ArgExpression (ConstString "users")
            , ArgColumn (Column "is_adult" TypeBool [])
            , ArgLambda
                (Lambda
                   (Conditional
                      (Expr (Ident "age") OperGreaterEquals (Ident "adult_age"))
                      (ConstBool True)
                      (ConstBool False)))
            ]
        , Declaration
            "teen_age"
            TypeInt
            (Expr (Ident "adult_age") OperSubtract (ConstInt 5))
        , FunctionCall
            OperationAdd
            [ ArgExpression (ConstString "users")
            , ArgColumn (Column "is_teen" TypeBool [])
            , ArgLambda
                (Lambda
                   (Conditional
                      (Expr (Ident "age") OperGreaterEquals (Ident "teen_age"))
                      (ConstBool True)
                      (ConstBool False)))
            ]
        , FunctionCall
            OperationRename
            [ ArgExpression (ConstString "users")
            , ArgExpression (ConstString "candidates")
            ]
        ]

exampleNorm :: Hasql
exampleNorm = Hasql init up
  where
    init =
      Init
        [ Table
            "users"
            [ Column "id" TypeInt [Primary]
            , Column "first_name" TypeString []
            , Column "age" TypeInt []
            ]
        ]
    up =
      Up
        [ FunctionCall
            OperationNormalize
            [ ArgExpression (ConstString "users")
            , ArgExpression (ConstString "ages")
            , ArgStringList ["age"]
            ]
        ]

exampleDecouple :: Hasql
exampleDecouple = Hasql init up
  where
    init =
      Init
        [ Table
            "users"
            [ Column "id" TypeInt [Primary]
            , Column "first_name" TypeString []
            , Column "age" TypeInt []
            ]
        ]
    up =
      Up
        [ FunctionCall
            OperationDecouple
            [ArgExpression (ConstString "users"), ArgStringList ["first_name"]]
        ]

exampleSplit :: Hasql
exampleSplit = Hasql init up
  where
    init =
      Init
        [ Table
            "users"
            [ Column "id" TypeInt [Primary]
            , Column "first_name" TypeString []
            , Column "age" TypeInt []
            ]
        ]
    up =
      Up
        [ FunctionCall
            OperationSplit
            [ ArgExpression (ConstString "users")
            , ArgExpression (ConstString "names")
            , ArgStringList ["first_name"]
            ]
        ]
