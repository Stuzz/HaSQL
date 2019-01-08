module Lib
  ( parseSql
  ) where

import Lexer
import Parser
import Syntax

-- | Transform a migration file into a list of SQL statements.
--
-- TODO: This should of course return SQL files for both the up and the down
--       migration.
parseSql :: String -> [String]
parseSql = check . parse . scan

-- | TODO: Placeholder function for the static checking and algebraic fold.
check :: Hasql -> [String]
check _ = [""]

example :: Hasql
example = Hasql init up
  where
    init = Init [
        Table "Users" [
          Column "ID" Int [Primary],
          Column "FirstName" String,
          Column "Age" Int,
      ]
    ]
    up = Up [
      FunctionCall OperationSplit [
        ArgExpression (Indent "Users"),
        ArgStringList ["FirstName"],
        ArgExpression (Indent "Names")
      ]
    ]
