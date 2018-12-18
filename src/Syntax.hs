-- | This module contains tokens and data types needed during the lexing and
-- parsing processes.
module Syntax where

data Token
  = TComma
  | TColon
  | TSemiColon
  | TBraceOpen
  | TLambda
  | TBraceClose
  | TParenOpen
  | TParenClose
  | TOperAdd
  | TOperSubtract
  | TOperMultiply
  | TOperDivide
  | TOperConcatenate
  | TOperEquals
  | TOperNotEquals
  | TOperLesserThan
  | TOperLesserEquals
  | TOperGreaterThan
  | TOperGreaterEquals
  | TFuncAdd
  | TFuncSplit
  | TFuncDecouple
  | TFuncNormalize
  | TFuncRename
  | TTypeBool
  | TTypeInt
  | TTypeString
  | TTypePrimary
  | TIdent String
  | TString String
  deriving (Show, Eq)
