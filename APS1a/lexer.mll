{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

rule token = parse
  [' ' '\t' '\n' '\r']           { token lexbuf }     (* skip blanks *)
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
  | "var"            { VARADR }
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
  | "adr"            { ADR }
  (*Constantes numeriques*)
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  (*Identificateurs*)
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
  | _ as char        { 
    let pos = lexbuf.lex_curr_p in
    let msg = Printf.sprintf "Lexing error: unexpected character '%c' at line %d, position %d" 
                             char pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    raise (Failure msg)
  }
