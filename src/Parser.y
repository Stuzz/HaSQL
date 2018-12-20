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
  ";"         { TSemiColon }
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
  "Add"       { TFuncAdd }
  "Split"     { TFuncSplit }
  "Decouple"  { TFuncDecouple }
  "Normalize" { TFuncNormalize }
  "Rename"    { TFuncRename }
  "Bool"      { TTypeBool }
  "Int"       { TTypeInt }
  "String"    { TTypeString }
  "Primary"   { TTypePrimary }
  Ident       { TIdent $$ }
  String      { TString $$ }
  Bool        { TBool $$ }
  Int         { TInt $$ }

%%

Block : "{" Ident "}" { $2 }

{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error: " ++ show tokens
}
