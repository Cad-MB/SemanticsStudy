type token =
  | NUM of (int)
  | IDENT of (string)
  | LBRA
  | RBRA
  | LPAR
  | RPAR
  | CONST
  | FUN
  | REC
  | IF
  | ECHO
  | SEMCOL
  | COL
  | COMA
  | ARROW
  | STAR
  | AND
  | OR
  | BOOL
  | INT

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmd list
