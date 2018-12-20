{
module Lexer where

import Syntax
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@ident = [$alpha \_] [$alpha $digit \_ \$]*

-- TODO: Add tokens for foreign keys after we have figured out how to handle
--       those

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
  "Up"           { const $ const TUp }
  "Init"         { const $ const TInit }
  "Bool"         { const $ const TTypeBool }
  "Int"          { const $ const TTypeInt }
  "String"       { const $ const TTypeString }
  "Primary"      { const $ const TTypePrimary }
  "Add"          { const $ const TFuncAdd }
  "Split"        { const $ const TFuncSplit }
  "Decouple"     { const $ const TFuncDecouple }
  "Rename"       { const $ const TFuncRename }
  "True"         { const $ const $ TBool True }
  "False"        { const $ const $ TBool False }
  \" ([^\"]+) \" { \_ (_:s) -> TString $ init s }
  $digit+        { \_ s     -> TInt $ read s }
  @ident         { \_ s     -> TIdent s }

{
-- | Run the scanner on a string.
scan :: String -> [Token]
scan = alexScanTokens
}
