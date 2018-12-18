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
  "{"            { const $ const TBraceOpen }
  "}"            { const $ const TBraceClose }
  \" ([^\"]+) \" { \_ (_:s) -> TString $ init s }
  @ident         { \_ s -> TIdent s }

{
-- | Run the scanner on a string.
scan :: String -> [Token]
scan = alexScanTokens
}
