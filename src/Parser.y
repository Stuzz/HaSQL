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
        | "primary" Ident ":" Type                        { Column $2 $4 [Primary] }
        | "foreign" Ident ":" Type                        { Column $2 $4 [Foreign] }

Statements   : {- empty -}                                { [] }
             | Statement ";" Statements                   { $1 : $3 }
Statement    : Declaration                                { $1 }
             | FunctionCall                               { $1 }
             | Assignment                                 { $1 }
Declaration  : Type Ident "=" Expression                  { Declaration $2 $1 $4 }
Assignment   : Ident "=" Expression                       { Assignment $1 $3 }
FunctionCall : Operation "(" Arguments ")"                { FunctionCall $1 $3 }
Operation    : "Add"                                      { OperationAdd }
             | "Split"                                    { OperationSplit }
             | "Decouple"                                 { OperationDecouple }
             | "Rename"                                   { OperationRename }
             | "Normalize"                                { OperationNormalize }

Arguments : {- empty -}                                   { [] }
          | Argument                                      { [$1] }
          | Argument "," Arguments                        { $1 : $3 }
Argument  : Expression                                    { ArgExpression $1 }
          | Lambda                                        { ArgLambda $1 }
          | Column                                        { ArgColumn $1 }
          | "[" Idents "]"                                { ArgStringList $2 }

Lambda         : "\\" Expression                          { Lambda $2 }
Expression     : Expression ThirdOperator SExpression     { Expr $1 $2 $3 }
               | SExpression                              { $1 }
               | Expression "?" Expression ":" Expression { Conditional $1 $3 $5 }
SExpression    : SExpression SecondOperator SExpression   { Expr $1 $2 $3 }
               | PExpression                              { $1 }
PExpression    : PExpression FirstOperator PExpression    { Expr $1 $2 $3 }
               | LExpression                              { $1 }
LExpression    : Ident                                    { Ident $1 }
               | Int                                      { ConstInt $1 }
               | Bool                                     { ConstBool $1 }
               | String                                   { ConstString $1 }
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

Type   : "Bool"                                           { TypeBool }
       | "Int"                                            { TypeInt }
       | "String"                                         { TypeString }
Idents : {- idents -}                                     { [] }
       | Ident                                            { [$1] }
       | Ident "," Idents                                 { $1 : $3 }

{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error: " ++ show tokens
}
