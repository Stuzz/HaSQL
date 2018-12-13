{
module Parser where

import Syntax
}

%name parse
%tokentype { Token }

%token
  "{"   { TBraceOpen }
  "}"   { TBraceClose }
  Ident { TIdent $$ }

%%

Block : "{" Ident "}" { $1 }

{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error: " ++ show tokens
}
