(* ************************************
            Type Declarations
   ************************************ *)

type id = Id of string * int

type term =
  [ `GeneralVar of id
  | `TableVar of id
  | `EmptyTable
  | `Table of term * table
  | `Predicate of string * term list ]

and table = [ `TableVar of id | `EmptyTable | `Table of term * table ]

type var = [ `GeneralVar of id | `TableVar of id ]

type clause = Clause of term * term list

type subst =
  | GeneralSubst of [ `GeneralVar of id ] * term
  | TableSubst of [ `TableVar of id ] * table

type substitution = subst list

type token =
  | PredicateString of string
  | VariableString of string
  | LeftParenthesis
  | RightParenthesis
  | LeftBracket
  | RightBracket
  | Vertical
  | Comma
  | Period
  | If
  | EndOfFile

(* ************************************
            Pretty Printing Functions
   ************************************ *)

let rec string_of_term : term -> string = function
  | `GeneralVar (Id (s, _)) -> if s.[0] = '_' then "_" else s
  | `TableVar (Id (s, _)) -> s
  | (`EmptyTable | `Table _) as t -> "[" ^ string_of_tblcontent t ^ "]"
  | `Predicate (s, []) -> s
  | `Predicate (s, l) -> s ^ "(" ^ (l |> List.map string_of_term |> String.concat ", ") ^ ")"

and string_of_tblcontent : [ `EmptyTable | `Table of term * table ] -> string = function
  | `EmptyTable -> ""
  | `Table (t, tail) -> (
      match tail with
      | `EmptyTable -> string_of_term t
      | `Table _ as tbl -> string_of_term t ^ ", " ^ string_of_tblcontent tbl
      | `TableVar (Id (s, _)) -> string_of_term t ^ " | " ^ s)

let string_of_clause (Clause (t, tl)) =
  string_of_term t
  ^ (if tl = [] then ""
    else " :-" ^ (tl |> List.map (fun t -> "\n  " ^ string_of_term t) |> String.concat ","))
  ^ "."

let string_of_program cl = cl |> List.map string_of_clause |> String.concat "\n\n"

let string_of_token = function
  | PredicateString s -> s
  | VariableString s -> s
  | LeftParenthesis -> "("
  | RightParenthesis -> ")"
  | LeftBracket -> "["
  | RightBracket -> "]"
  | Vertical -> "|"
  | Comma -> ","
  | Period -> "."
  | If -> ":-"
  | EndOfFile -> "EOF"

let string_of_tokenlist tl = tl |> List.map string_of_token |> String.concat "; "

(* ************************************
            Tokenizer
   ************************************ *)

exception TokenNotFound of char

let rec parse_word = function
  | (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9'| '_') as c) :: t ->
      let w, t' = parse_word t in
      (c :: w, t')
  | l -> ([], l)

let string_of_charlist cl = String.of_seq (List.to_seq cl)

let rec tokenlist_of_charlist : token list -> char list -> token list =
 fun acc -> function
  | (' ' | '\t' | '\n') :: t -> tokenlist_of_charlist acc t
  | '[' :: t -> tokenlist_of_charlist (LeftBracket :: acc) t
  | ']' :: t -> tokenlist_of_charlist (RightBracket :: acc) t
  | '(' :: t -> tokenlist_of_charlist (LeftParenthesis :: acc) t
  | ')' :: t -> tokenlist_of_charlist (RightParenthesis :: acc) t
  | '|' :: t -> tokenlist_of_charlist (Vertical :: acc) t
  | ',' :: t -> tokenlist_of_charlist (Comma :: acc) t
  | '.' :: t -> tokenlist_of_charlist (Period :: acc) t
  | ':' :: '-' :: t -> tokenlist_of_charlist (If :: acc) t
  | '_' :: t -> tokenlist_of_charlist (VariableString "_" :: acc) t
  | [] -> List.rev (EndOfFile :: acc)
  | ('a' .. 'z' as low) :: t ->
      let w, cl = parse_word t in
      tokenlist_of_charlist (PredicateString (string_of_charlist (low :: w)) :: acc) cl
  | ('A' .. 'Z' as up) :: t ->
      let w, cl = parse_word t in
      tokenlist_of_charlist (VariableString (string_of_charlist (up :: w)) :: acc) cl
  | tok :: _ -> raise (TokenNotFound tok)

let charlist_of_string s = List.init (String.length s) (String.get s)

let tokenlist_of_string s =
  try Ok (s |> charlist_of_string |> tokenlist_of_charlist [])
  with TokenNotFound tok -> Error ("this token is unknown : '" ^ String.make 1 tok ^ "'")

(* ************************************
            Parsing
   ************************************ *)

type 'a parser = token list -> ('a * token list, string) result

let ( >>= ) = Result.bind

let ( *~ ) : 'a parser -> 'b parser -> ('a * 'b) parser =
 fun p q cl ->
  p cl >>= fun (a, cl') ->
  q cl' >>= fun (b, cl'') -> Ok ((a, b), cl'')

let ( *> ) : 'a parser -> ('a -> 'b) -> 'b parser =
 fun p f cl -> p cl >>= fun (a, cl') -> Ok (f a, cl')

let ignore_left (_, b) = b

let ignore_right (a, _) = a

let epsilon tl = Ok ((), tl)

let parse_token tk = function
  | t :: l when t = tk -> Ok (t, l)
  | _ -> Error ("Unable to parse token : " ^ string_of_token tk)

let look_ahead tl f =
  match tl with [] -> Error "Unexpected end of token stream." | tok :: toklist -> f (tok, toklist)

let rec table_of_termlist : table -> term list -> table =
 fun tail -> function [] -> tail | h :: t -> `Table (h, table_of_termlist tail t)

let rec parse_term tl =
  look_ahead tl (function
    | VariableString s, l -> Ok (`GeneralVar (Id (s, 0)), l)
    | PredicateString s, l ->
        l |> parse_predicate_without_string *> fun term_list -> `Predicate (s, term_list)
    | LeftBracket, l -> l |> parse_table_without_left_bracket *> fun tbl -> (tbl :> term)
    | tok, _ -> Error ("Not a valid term : " ^ string_of_token tok))

and parse_predicate_without_string tl =
  look_ahead tl (function
    | LeftParenthesis, l -> l |> parse_term_list *~ parse_token RightParenthesis *> ignore_right
    | _, _ -> Ok ([], tl))

and parse_term_list tl =
  tl |> parse_term *~ parse_term_list_without_first_term *> fun (t, tl') -> t :: tl'

and parse_term_list_without_first_term tl =
  look_ahead tl (function
    | Comma, l -> l |> parse_term *~ parse_term_list_without_first_term *> fun (a, b) -> a :: b
    | _, _ -> Ok ([], tl))

and parse_table_without_left_bracket tl =
  look_ahead tl (function
    | RightBracket, l -> Ok (`EmptyTable, l)
    | _, _ ->
        tl
        |> parse_term_list *~ parse_table_without_term_list *> fun (termlist, table) ->
           table_of_termlist table termlist)

and parse_table_without_term_list tl =
  look_ahead tl (function
    | RightBracket, l -> Ok (`EmptyTable, l)
    | Vertical, l -> l |> parse_table_queue
    | tok, _ -> Error ("Invalid right part of the table : " ^ string_of_token tok))

and parse_table_queue tl =
  look_ahead tl (function
    | LeftBracket, l ->
        l |> parse_table_without_left_bracket *~ parse_token RightBracket *> ignore_right
    | VariableString s, l -> l |> parse_token RightBracket *> fun _ -> `TableVar (Id (s, 0))
    | _ -> Error "Invalid table queue")

let parse_right_clause tl =
  look_ahead tl (function
    | Period, l -> Ok ([], l)
    | _, _ ->
        tl |> parse_token If *~ parse_term_list *> ignore_left *~ parse_token Period *> ignore_right)

let parse_clause tl = tl |> parse_term *~ parse_right_clause *> fun (t, rc) -> Clause (t, rc)

let rec parse_program tl =
  look_ahead tl (function
    | EndOfFile, _ -> Ok ([], [])
    | _, _ -> tl |> parse_clause *~ parse_program *> fun (a, b) -> a :: b)

let parse_request tl = tl |> parse_term_list *~ parse_token EndOfFile *> ignore_right

(* ************************************
            Substitution Functions
   ************************************ *)

let map_vars f g =
  let rec mapterm : term -> term = function
    | #table as tbl -> (maptable tbl :> term)
    | `GeneralVar _ as v -> f v
    | `Predicate (s, l) -> `Predicate (s, List.map mapterm l)
  and maptable : table -> table = function
    | `EmptyTable -> `EmptyTable
    | `TableVar _ as v -> g v
    | `Table (h, t) -> `Table (mapterm h, maptable t)
  in
  mapterm

let apply_subst : subst -> term -> term =
  let id x = x in
  function
  | GeneralSubst (var, t) -> map_vars (function v -> if var = v then t else (v :> term)) id
  | TableSubst (var, t) -> map_vars id (function v -> if var = v then t else (v :> table))

(* De droite à gauche *)
let apply (s : substitution) term = List.fold_right apply_subst s term

(* ************************************
            Unification Algorithm
   ************************************ *)

let rec unify : term -> term -> substitution option =
 fun t1 t2 ->
  let f ta tb s1 = Option.bind (unify (apply s1 ta) (apply s1 tb)) (fun s2 -> Some (s2 @ s1)) in
  match (t1, t2) with
  | (`TableVar _ as v), ((`TableVar _ | `EmptyTable | `Table _) as t) -> Some [ TableSubst (v, t) ]
  | (`EmptyTable | `Table _), `TableVar _ -> unify t2 t1
  | `EmptyTable, `EmptyTable -> Some []
  | `Table (h1, t1), `Table (h2, t2) -> Option.bind (unify h1 h2) (f (t1 :> term) (t2 :> term))
  | `Table _, `EmptyTable | `EmptyTable, `Table _ -> None
  | (`GeneralVar _ as v), _ -> Some [ GeneralSubst (v, t2) ]
  | _, `GeneralVar _ -> unify t2 t1
  | `Predicate (sa, la), `Predicate (sb, lb) ->
      if sa <> sb then None
      else if List.length la <> List.length lb then None
      else List.fold_left2 (fun opt_s ta tb -> Option.bind opt_s (f ta tb)) (Some []) la lb
  | (`TableVar _ | `EmptyTable | `Table _), `Predicate _
  | `Predicate _, (`TableVar _ | `EmptyTable | `Table _) ->
      None

(* ************************************
            Type Inference
   ************************************ *)

let rec variables_in_term = function
  | `GeneralVar id -> [ (`GeneralVar id : var) ]
  | `TableVar id -> [ `TableVar id ]
  | `EmptyTable -> []
  | `Table (h, t) -> variables_in_term h @ variables_in_term (t :> term)
  | `Predicate (_, l) -> l |> List.map variables_in_term |> List.concat

let variables_in_clause (Clause (t, l)) = List.concat_map variables_in_term (t :: l)

let variables_in_request r = List.concat_map variables_in_term r

let replace_tvars_in_term tvars =
  map_vars
    (fun (`GeneralVar id) -> if List.mem (`TableVar id) tvars then `TableVar id else `GeneralVar id)
    (fun x -> x)

let type_inference_clause (Clause (t, l) as c) =
  let f = replace_tvars_in_term (variables_in_clause c) in
  Clause (f t, List.map f l)

let type_inference_request r =
  let f = replace_tvars_in_term (variables_in_request r) in
  List.map f r

let type_inference_program = List.map type_inference_clause

(* ************************************
            Solver
   ************************************ *)

let rename n (Clause (t, l)) =
  let f =
    map_vars
      (fun (`GeneralVar (Id (s, _))) -> `GeneralVar (Id (s, n)))
      (fun (`TableVar (Id (s, _))) -> `TableVar (Id (s, n)))
  in
  Clause (f t, List.map f l)

type 'a tree = Leaf of 'a | Node of 'a tree Lazy.t list

let rec sld_tree world request substitution n =
  match request with
  | [] -> Leaf substitution
  | head_request_term :: other_request_terms ->
      let filter_clause c =
        let (Clause (left_member, right_member)) = rename n c in
        let new_tree unifier =
          lazy
            (sld_tree world
               (List.map (apply unifier) (right_member @ other_request_terms))
               (unifier @ substitution) (n + 1))
        in
        Option.map new_tree (unify head_request_term left_member)
      in
      Node (List.filter_map filter_clause world)

let list_to_seq l = List.fold_right (fun x s () -> Seq.Cons (x, s)) l Seq.empty

let rec to_seq = function
  | Leaf str -> Seq.return str
  | Node tl -> Seq.flat_map (fun par -> to_seq (Lazy.force par)) (list_to_seq tl)

let solutions tree vars =
  Seq.map (fun substitution -> (vars, List.map (apply substitution) vars)) (to_seq tree)

(* ************************************
            Interface Functions
   ************************************ *)

let program_of_string s =
  s |> tokenlist_of_string >>= parse_program >>= fun (p, _) -> Ok (type_inference_program p)

let request_of_string s =
  s |> tokenlist_of_string >>= parse_request >>= fun (r, _) -> Ok (type_inference_request r)

let string_sequence_of parsed_program parsed_request =
  let vars = List.concat_map variables_in_term parsed_request |> List.sort_uniq compare in
  let sol = solutions (sld_tree parsed_program parsed_request [] 1) (vars :> term list) in
  let f (vars_tl, tl) =
    if vars = [] then "This is true."
    else
      "A solution is : "
      ^ (List.map2 (fun v t -> string_of_term v ^ " = " ^ string_of_term t) vars_tl tl
        |> String.concat ", ")
  in
  Seq.map f sol

let basic_interpreter program_string request_string =
  match program_of_string program_string with
  | Error s ->
      print_endline "Unable to parse program :";
      print_endline s
  | Ok parsed_program ->
      (match request_of_string request_string with
      | Error s ->
          print_endline "Unable to parse request :";
          print_endline s
      | Ok parsed_request -> (
          let string_seq = string_sequence_of parsed_program parsed_request in
          match string_seq () with
          | Seq.Nil -> print_endline "No solution found."
          | Seq.Cons (sol, seq) ->
              print_endline sol;
              Seq.iter print_endline seq));
      print_newline ()

(* ************************************
            Benchmark Functions
   ************************************ *)

let read_lines name =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  String.concat "\n" (loop [])
(*
let () =
  let name = Sys.argv.(1) in
  print_endline ("reading file : " ^ name);
  let prg_string = read_lines name in
  for _j = 1 to 1 do
    let i = 10000 in
    let big_prg_string = List.init i (fun _ -> prg_string) |> String.concat "\n" in
    let t = Unix.gettimeofday () in
    match big_prg_string |> program_of_string with
    | Ok _ ->
        let t' = Unix.gettimeofday () in
        Format.sprintf "%ibytes : %gs  ~%g MB/s." (String.length big_prg_string) (t' -. t)
          (float_of_int (String.length big_prg_string) /. (t' -. t) /. 1e6)
        |> print_endline
    | Error e -> print_endline e
  done
*)