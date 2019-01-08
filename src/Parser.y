{
module Parser where

import Syntax
}

%name parse
%tokentype { Token }

%token
  ","         { TComma }
  ":"         { TColon }
  "?"         { TQuestionMark }
  "\\"        { TLambda }
  "="         { TAssignment }
  ";"         { TSemiColon }
  "["         { TBracketOpen }
  "]"         { TBracketClose }
  "{"         { TBraceOpen }
  "}"         { TBraceClose }
  "("         { TParenOpen }
  ")"         { TParenClose }
  "+"         { TOperAdd }
  "-"         { TOperSubtract }
  "*"         { TOperMultiply }
  "/"         { TOperDivide }
  "++"        { TOperConcatenate }
  "=="        { TOperEquals }
  "!="        { TOperNotEquals }
  "<"         { TOperLesserThan }
  "<="        { TOperLesserEquals }
  ">"         { TOperGreaterThan }
  ">="        { TOperGreaterEquals }
  "up"        { TUp }
  "init"      { TInit }
  "Table"     { TTable }
  "Add"       { TFuncAdd }
  "Split"     { TFuncSplit }
  "Decouple"  { TFuncDecouple }
  "Normalize" { TFuncNormalize }
  "Rename"    { TFuncRename }
  "Bool"      { TTypeBool }
  "Int"       { TTypeInt }
  "String"    { TTypeString }
  "primary"   { TTypePrimary }
  "foreign"   { TTypeForeign }
  Ident       { TIdent $$ }
  String      { TString $$ }
  Bool        { TBool $$ }
  Int         { TInt $$ }

%%

Hasql : Init Up                            { Hasql $1 $2 }
Init  : "init" "{" Tables "}"              { Init $3 }
Up    : "up" "{" Statements "}"            { Up $3 }

Tables : {- empty -}                       { [] }
       | Table Tables                      { $1 : $2 }
Table  : "Table" Ident "{" Columns "}"     { Table $2 $4 }

Columns : {- empty -}                      { [] }
        | Column ";" Columns               { $1 : $3 }
Column  : Ident ":" Type                   { Column $1 $3 [] }
        | "primary" Ident ":" Type         { Column $1 $3 [Primary] }
        | "foreign" Ident ":" Type         { Column $1 $3 [Foreign] }
Type    : "Bool"                           { TTypeBool }
        | "Int"                            { TTypeInt }
        | "String"                         { TTypeString }

Statements : {- empty -}                   { [] }
           | Statement ";" Statements      { $1 : $3 }
Statement  : Declaration                   { $$ }
           | FunctionCall                  { $$ }
           | Assignment                    { $$ }
Declaration : Type Ident                   { Declaration $1 $2 Undefined }
            | Type Ident "=" Expression    { Declaration $1 $2 $4 }
FunctionCall : Operation "(" Arguments ")" { FunctionCall $1 $3 }
Operation : "Add"                          { OperationAdd }
          | "Split"                        { OperationSplit }
          | "Decouple"                     { OperationDecouple }
          | "Rename"                       { OperationRename }
          | "Normalize"                    { OperationNormalize }


Arguments : {- empty -}                    { [] }
          | Argument                       { [$1] }
          | Argument "," Arguments         { $1 : $3 }
Argument : Expression                      { ArgExpression $$ }
         | Lambda                          { ArgLambda $$ }
         | Column                          { ArgColumn $$ }
         | "[" Idents "]"                  { ArgStringList $2 }

{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error: " ++ show tokens
}
