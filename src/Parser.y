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

Hasql : Init Up                                           { Hasql $1 $2 }
Init  : "init" "{" Tables "}"                             { Init $3 }
Up    : "up" "{" Statements "}"                           { Up $3 }

Tables : {- empty -}                                      { [] }
       | Table Tables                                     { $1 : $2 }
Table  : "Table" Ident "{" Columns "}"                    { Table $2 $4 }

Columns : {- empty -}                                     { [] }
        | Column ";" Columns                              { $1 : $3 }
Column  : Ident ":" Type                                  { Column $1 $3 [] }
        | "primary" Ident ":" Type                        { Column $1 $3 [Primary] }
        | "foreign" Ident ":" Type                        { Column $1 $3 [Foreign] }

Statements   : {- empty -}                                { [] }
             | Statement ";" Statements                   { $1 : $3 }
Statement    : Declaration                                { $$ }
             | FunctionCall                               { $$ }
             | Assignment                                 { $$ }
Declaration  : Type Ident                                 { Declaration $1 $2 Undefined }
             | Type Ident "=" Expression                  { Declaration $1 $2 $4 }
Assignment   : Ident "=" Expression                       { Assignment $1 $3 }
FunctionCall : Operation "(" Arguments ")"                { FunctionCall $1 $3 }
Operation    : "Add"                                      { OperationAdd }
             | "Split"                                    { OperationSplit }
             | "Decouple"                                 { OperationDecouple }
             | "Rename"                                   { OperationRename }
             | "Normalize"                                { OperationNormalize }

Arguments : {- empty -}                                   { [] }
          | Argument                                      { [$$] }
          | Argument "," Arguments                        { $1 : $3 }
Argument  : Expression                                    { ArgExpression $$ }
          | Lambda                                        { ArgLambda $$ }
          | Column                                        { ArgColumn $$ }
          | "[" Idents "]"                                { ArgStringList $2 }

Lambda         : "\\" Expression                          { Lambda $2 }
Expression     : Expression ThirdOperator SExpression     { Expr $1 $2 $3 }
               | SExpression                              { $$ }
               | Expression "?" Expression ":" Expression { Conditional $1 $3 $5 }
SExpression    : SExpression SecondOperator SExpression   { Expr $1 $2 $3 }
               | PExpression                              { $$ }
PExpression    : PExpression FirstOperator PExpression    { Expr $2 $2 $3 }
               | LExpression                              { $$ }
LExpression    : Ident                                    { Ident $$ }
               | Bool                                     { ConstBool $$ }
               | String                                   { ConstString $$ }
               | "(" Expression ")"                       { $2 }
ThirdOperator  : "=="                                     { OperEquals }
               | "!="                                     { OperNotEquals }
               | "<"                                      { OperLesserThan }
               | ">"                                      { OperGreaterThan }
               | "<="                                     { OperLesserEquals }
               | ">="                                     { OperGreaterEquals }
               | "++"                                     { OperConcatenate }
SecondOperator : "+"                                      { OperAdd }
               | "-"                                      { OperSubtract }
FirstOperator  : "/"                                      { OperDivide }
               | "*"                                      { OperMultiply }

Type   : "Bool"                                           { TTypeBool }
       | "Int"                                            { TTypeInt }
       | "String"                                         { TTypeString }
Idents : {- idents -}                                     { [] }
       | Ident                                            { [$$] }
       | Ident "," Idents                                 { $1 : $3 }

{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error: " ++ show tokens
}
