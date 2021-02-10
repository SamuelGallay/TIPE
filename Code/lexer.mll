{
  open Parser
  exception Error of string
}

let char = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule mylexer = parse
| [' ' '\t' '\n'] { mylexer lexbuf }
| '.' { STOP }
| ":-" { IF }
| ',' { AND }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRA }
| ']' { RBRA }
| '|' { TAIL }
| '?' { ASK }
| '_' { NVAR }
| ['A'-'Z'] char* as s { VAR s }
| ['a'-'z'] char* as s { PRE s }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
