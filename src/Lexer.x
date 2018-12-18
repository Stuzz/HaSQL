{
module Lexer where

import Syntax
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@ident = [$alpha \_] [$alpha $digit \_ \$]*

tokens :-
  $white+        ;
  \#.*           ; -- Comments
  \,             { const $ const TComma }
  \:             { const $ const TColon }
  \;             { const $ const TSemiColon }
  \?             { const $ const TQuestionMark }
  \\             { const $ const TLambda }
  \{             { const $ const TBraceOpen }
  \}             { const $ const TBraceClose }
  \(             { const $ const TParenOpen }
  \)             { const $ const TParenClose }
  \+             { const $ const TOperAdd }
  \-             { const $ const TOperSubtract }
  \*             { const $ const TOperMultiply }
  \/             { const $ const TOperDivide }
  "++"           { const $ const TOperConcatenate }
  "=="           { const $ const TOperEquals }
  "!="           { const $ const TOperNotEquals }
  \<             { const $ const TOperLesserThan }
  "<="           { const $ const TOperLesserEquals }
  \>             { const $ const TOperGreaterThan }
  ">="           { const $ const TOperGreaterEquals }
  "Bool"         { const $ const TTypeBool }
  "Int"          { const $ const TTypeInt }
  "String"       { const $ const TTypeString }
  "Primary"      { const $ const TTypePrimary }
  "Add"          { const $ const TFuncAdd }
  "Split"        { const $ const TFuncSplit }
  "Decouple"     { const $ const TFuncDecouple }
  "Rename"       { const $ const TFuncRename }
  \" ([^\"]+) \" { \_ (_:s) -> TString $ init s }
  @ident         { \_ s     -> TIdent s }

{
-- | Run the scanner on a string.
scan :: String -> [Token]
scan = alexScanTokens
}
