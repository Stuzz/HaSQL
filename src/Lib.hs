module Lib
  ( parseSql
  ) where

import Algebra
import Lexer
import Parser
import qualified Static
import Syntax

-- | Transform a migration file into a list of SQL statements.
--
-- TODO: This should of course return SQL files for both the up and the down
--       migration.
parseSql :: String -> [String]
parseSql = const [] . Static.check . parse . scan

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
            [ ArgExpression (Ident "Users")
            , ArgStringList ["FirstName"]
            , ArgExpression (Ident "Names")
            ]
        ]
