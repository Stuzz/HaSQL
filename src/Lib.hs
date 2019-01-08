module Lib
  ( someFunc
  )
where

import           Lexer
import           Parser
import           Syntax

someFunc :: IO ()
someFunc = getContents >>= print . parse . scan

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