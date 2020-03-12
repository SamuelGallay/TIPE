type var = Id of string * int;;

type atom = Atom of string;;

type term = Var of var | Term of atom * (term list);;

type clause = Clause of term * (term list);;

type token = TLPar | TRPar | TDot | TIf | TAnd | TVar of string | TAtom of string;;

type 'a tree = Leaf of 'a | Node of (('a tree) Lazy.t) list;;

val lexer : string -> token list;;

val parser : string -> clause list;;

val parser_term : string -> term;;

val parser_term_list : string -> term list;;

val parser_clause : string -> clause;;

val solutions : clause list -> string -> (term list * term list) Seq.t;;

val request : clause list -> string -> unit;;
