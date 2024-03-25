(*Lexique: string -> lexemes*)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
    (*Symboles reserves*)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { SEMCOL }
  | ':'              { COL }
  | ','              { COMA }
  | '*'              { STAR }
  | "->"             { ARROW }
  (*Mots clef*)
  | "CONST"          { CONST }
  | "FUN"            { FUN }
  | "REC"            { REC }
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "ECHO"           { ECHO } 
  | "SET"            { SET } 
  | "WHILE"          { WHILE } 
  | "CALL"           { CALL } 
  | "void"           { VOID } 
  | "if"             { IF }
  | "and"            { AND }
  | "or"             { OR }
  | "bool"           { BOOL }
  | "int"            { INT }
  (*Constantes numeriques*)
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  (*Identificateurs*)
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }