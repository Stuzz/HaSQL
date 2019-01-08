module Lib
  ( parseSql
  ) where

import Lexer
import Parser
import Syntax
import Algebra

-- | Transform a migration file into a list of SQL statements.
--
-- TODO: This should of course return SQL files for both the up and the down
--       migration.
parseSql :: String -> [String]
parseSql = undefined
  -- check . parse . scan

-- | TODO: Placeholder function for the static checking and algebraic fold.
check :: Hasql -> [String]
check _ = [""]

example :: Hasql
example = Hasql init up
  where
    init = Init [
        Table "Users" [
          Column "ID" TypeInt [Primary],
          Column "FirstName" TypeString [],
          Column "Age" TypeInt []
        ]
      ]
    up = Up [
      FunctionCall OperationSplit [
        ArgExpression (Ident "Users"),
        ArgStringList ["FirstName"],
        ArgExpression (Ident "Names")
        ]
      ]
