module Lib
  ( parseSql
  ) where

import Debug.Trace

import Algebra
import qualified Dynamic
import Lexer
import Parser
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
