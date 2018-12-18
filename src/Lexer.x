{
module Lexer where

import Syntax
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@ident = [$alpha \_] [$alpha $digit \_ \$]*

tokens :-
  $white+;
  \#.*           ; -- Comments
  \,             { const $ const TComma }
  \{             { const $ const TBraceOpen }
  \}             { const $ const TBraceClose }
  \(             { const $ const TParenOpen }
  \)             { const $ const TParenClose }
  \+             { const $ const TOperAdd }
  \-             { const $ const TOperSubtract }
  \*             { const $ const TOperMultiply }
  \/             { const $ const TOperDivide }
  "++"           { const $ const TOperConcatate }
  "=="           { const $ const TOperEquals }
  "!="           { const $ const TOperNotEquals }
  \<             { const $ const TOperLesserThan }
  "<="           { const $ const TOperLesserEquals }
  \>             { const $ const TOperGreaterThan }
  ">="           { const $ const TOperGreaterEquals }
  \" ([^\"]+) \" { \_ (_:s) -> TString $ init s }
  @ident         { \_ s -> TIdent s }

{
-- | Run the scanner on a string.
scan :: String -> [Token]
scan = alexScanTokens
}
