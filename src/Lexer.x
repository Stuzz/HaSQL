{
module Lexer where

import Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                      ;
  "#".*                        ; -- Comments
  "{"                          { const TBraceOpen }
  "}"                          { const TBraceClose }
  $alpha [$alpha $digit \_ \-] { \s -> TIdent s }

{
-- | Run the scanner on a string.
scan :: String -> [Token]
scan = alexScanTokens
}
