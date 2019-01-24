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
            "Users"
            [ Column "ID" TypeInt [Primary]
            , Column "FirstName" TypeString []
            , Column "Age" TypeInt []
            ]
        ]
    up =
      Up
        [ FunctionCall
            OperationSplit
            [ ArgExpression (ConstString "Users")
            , ArgExpression (ConstString "Names")
            , ArgStringList ["FirstName"]
            ]
        ]

exampleAdd :: Hasql
exampleAdd = Hasql init up
  where
    init =
      Init
        [ Table
            "Users"
            [ Column "ID" TypeInt [Primary]
            , Column "FirstName" TypeString []
            , Column "Age" TypeInt []
            ]
        ]
    up =
      Up
        [ Declaration "AdultAge" TypeInt (ConstInt 18)
          , FunctionCall
            OperationAdd
            [ ArgExpression (ConstString "Users")
            , ArgColumn (Column "IsAdult" TypeString [])
            , ArgLambda (Lambda
                (Conditional (Expr (Ident "Age") OperGreaterEquals (Ident "AdultAge"))
                  (ConstBool True) (ConstBool False)))
            ]
          , Declaration "TeenAge" TypeInt (Expr (Ident "AdultAge") OperSubtract (ConstInt 5))
          , FunctionCall
            OperationAdd
            [ ArgExpression (ConstString "Users")
            , ArgColumn (Column "IsTeen" TypeString [])
            , ArgLambda (Lambda
                (Conditional (Expr (Ident "Age") OperGreaterEquals (Ident "TeenAge"))
                  (ConstBool True) (ConstBool False)))
            ]
          , FunctionCall
            OperationRename
            [ ArgExpression (ConstString "Users")
            , ArgExpression (ConstString "Candidates")
            ]
        ]


exampleNorm :: Hasql
exampleNorm = Hasql init up
  where
    init =
      Init
        [ Table
            "Users"
            [ Column "ID" TypeInt [Primary]
            , Column "FirstName" TypeString []
            , Column "Age" TypeInt []
            ]
        ]
    up =
      Up
        [ FunctionCall
            OperationNormalize
            [ ArgExpression (ConstString "Users")
            , ArgExpression (ConstString "Ages")
            , ArgStringList ["FirstName"]
            ]
        ]


exampleDecouple :: Hasql
exampleDecouple = Hasql init up
  where
    init =
      Init
        [ Table
            "Users"
            [ Column "ID" TypeInt [Primary]
            , Column "FirstName" TypeString []
            , Column "Age" TypeInt []
            ]
        ]
    up =
      Up
        [ FunctionCall
            OperationDecouple
            [ ArgExpression (ConstString "Users")
            , ArgStringList ["FirstName"]
            ]
        ]
