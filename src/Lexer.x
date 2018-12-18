{
module Lexer where

import Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@ident = [$alpha \_] [$alpha $digit \_ \$]

tokens :-
  $white+                      ;
  "#".*                        ; -- Comments
  "{"                          { const TBraceOpen }
  "}"                          { const TBraceClose }
  @ident { \s -> TIdent s }

{
-- | Run the scanner on a string.
scan :: String -> [Token]
scan = alexScanTokens
}
