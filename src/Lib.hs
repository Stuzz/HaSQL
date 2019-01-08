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
