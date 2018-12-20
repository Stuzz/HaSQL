-- | This module contains tokens and data types needed during the lexing and
-- parsing processes.
module Syntax where

data Token
  = TComma
  | TColon
  | TQuestionMark
  | TLambda
  | TSemiColon
  | TBraceOpen
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
  | TUp
  | TInit
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
  | TBool Bool
  | TInt Int
  | TString String
  deriving (Show, Eq)

-- TODO: Add docs to every data type below (and probably also to some more
--       complicated constructors)
-- | The main entry point of a migration file.
data Hasql =
  Hasql Init
        Up

newtype Up =
  Up [Statement]

newtype Init =
  Init [Table]

-- | The schema for a named table.
data Table =
  Table String
        [Column]

-- | A column with the specified type. The column modifier list can be empty.
data Column =
  Column String
         Type
         [ColumnModifier]

-- | A single statement. A migration function consists of several statements.
data Statement
  -- ^ A declaration for a named variable with the value evaluated from the
  -- expression.
  = Declaration String
                Expression
  -- ^ An assignment to the named variable with the the value of the expression.
  | Assignment String
               Expression
  -- ^ A function call with the specified arguments.
  | FunctionCall Operation
                 [Argument]

data Argument
  = ArgExpression Expression
  | ArgLambda Lambda
  | ArgColumn Column
  | ArgStringList [String]

data ColumnModifier
  -- ^ The table's primary key.
  = Primary
  -- ^ TODO: Foreign keys relations can not be defined yet.
  | Foreign

newtype Lambda =
  Lambda Expression

data Expression
  = Expr Expression
         Operator
         Expression
  -- ^ A ternary conditional in the form 'if (Expression) then Expression else
  -- Expression'.
  | Conditional Expression
                Expression
                Expression
  | ConstString String
  | ConstBool Bool
  | ConstInt Int
  -- ^ A table or column identifier.
  | Ident String

data Operator
  = OperAdd
  | OperSubtract
  | OperMultiply
  | OperDivide
  | OperConcatenate
  | OperEquals
  | OperNotEquals
  | OperLesserThan
  | OperLesserEquals
  | OperGreaterThan
  | OperGreaterEquals

data Type
  = TypeBool
  | TypeString
  | TypeInt

data Operation
  = OperationAdd
  | OperationSplit
  | OperationDecouple
  | OperationNormalize
  | OperationRename
