-- | This module contains tokens and data types needed during the lexing and
-- parsing processes.
module Syntax where

data Token
  = TBraceOpen
  | TBraceClose
  | TIdent String
  | TString String
  deriving (Show, Eq)
