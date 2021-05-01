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

(* ************************************
            Pretty Printing Functions
   ************************************ *)

let rec string_of_term : term -> string = function
  | `GeneralVar (Id (s, _)) -> s
  | `TableVar (Id (s, _)) -> s
  | (`EmptyTable | `Table _) as t -> "[" ^ string_of_tblcontent t ^ "]"
  | `Predicate (s, []) -> s
  | `Predicate (s, l) ->
      s ^ "(" ^ (l |> List.map string_of_term |> String.concat ", ") ^ ")"

and string_of_tblcontent : [ `EmptyTable | `Table of term * table ] -> string =
  function
  | `EmptyTable -> ""
  | `Table (t, tail) -> (
      match tail with
      | `EmptyTable -> string_of_term t
      | `Table _ as tbl -> string_of_term t ^ ", " ^ string_of_tblcontent tbl
      | `TableVar (Id (s, _)) -> string_of_term t ^ " | " ^ s)

let string_of_clause (Clause (t, tl)) =
  string_of_term t
  ^ (if tl = [] then "" else " :- ")
  ^ (tl |> List.map string_of_term |> String.concat ", ")
  ^ "."

let _string_of_program cl =
  cl |> List.map string_of_clause |> String.concat "\n"

(* ************************************
            Debug Functions
   ************************************ *)

let rec string_of_term_debug : term -> string = function
  | `GeneralVar (Id (s, i)) -> "gv:" ^ s ^ string_of_int i
  | `TableVar (Id (s, i)) -> "tv:" ^ s ^ string_of_int i
  | (`EmptyTable | `Table _) as t -> "[" ^ string_of_tblcontent_debug t ^ "]"
  | `Predicate (s, []) -> s
  | `Predicate (s, l) ->
      s ^ "(" ^ (l |> List.map string_of_term_debug |> String.concat ", ") ^ ")"

and string_of_tblcontent_debug :
    [ `EmptyTable | `Table of term * table ] -> string = function
  | `EmptyTable -> ""
  | `Table (t, tail) -> (
      match tail with
      | `EmptyTable -> string_of_term_debug t
      | `Table _ as tbl ->
          string_of_term_debug t ^ ", " ^ string_of_tblcontent_debug tbl
      | `TableVar (Id (s, i)) ->
          string_of_term_debug t ^ " | " ^ "tv:" ^ s ^ string_of_int i)

let string_of_clause_debug (Clause (t, tl)) =
  string_of_term_debug t
  ^ (if tl = [] then "" else " :- ")
  ^ (tl |> List.map string_of_term_debug |> String.concat ", ")
  ^ "."

let _string_of_program_debug cl =
  cl |> List.map string_of_clause_debug |> String.concat "\n"

let print_substitution subs =
  print_string "Substitution : ";
  subs
  |> List.map (function
       | GeneralSubst (v, t) ->
           string_of_term_debug (v :> term) ^ " = " ^ string_of_term_debug t
       | TableSubst (v, t) ->
           string_of_term_debug (v :> term)
           ^ " = "
           ^ string_of_term_debug (t :> term))
  |> String.concat ", " |> print_endline

(* ************************************
            Parsing
   ************************************ *)

type 'a parser = char list -> (char list * 'a) list

let rec remove_spaces = function
  | [] -> []
  | (' ' | '\t' | '\n') :: l -> remove_spaces l
  | a :: l -> a :: remove_spaces l

let charlist_of_string s = List.init (String.length s) (String.get s)

let parse_char : char -> string parser =
 fun c -> function
  | [] -> []
  | h :: tl -> if h = c then [ (tl, String.make 1 c) ] else []

let ( +~ ) : 'a parser -> 'a parser -> 'a parser = fun p q cl -> p cl @ q cl

let ( *~ ) : 'a parser -> 'b parser -> ('a * 'b) parser =
 fun p q cl ->
  p cl
  |> List.map (fun (l, a) -> q l |> List.map (fun (l', b) -> (l', (a, b))))
  |> List.concat

let ( *>~ ) : 'a parser -> ('a -> 'b) -> 'b parser =
 fun p f cl -> p cl |> List.map (fun (l, a) -> (l, f a))

(* neutre pour +~ *)
let null_parser _cl = []

(* neutre pour *~ *)
let epsilon_parser cl = [ (cl, ()) ]

let any pl = List.fold_left ( +~ ) null_parser pl

let lower_case = "abcdefghijklmnopqrstuvwxyz"

let upper_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let other_chars = "0123456789_-"

let all_chars = lower_case ^ upper_case ^ other_chars

let parse_char_of s = charlist_of_string s |> List.map parse_char |> any

let rec parse_word cl =
  cl
  |> parse_char_of all_chars
     +~ parse_char_of all_chars *~ parse_word *>~ fun (a, b) -> a ^ b

let parse_predicate_string =
  parse_char_of lower_case
  +~ parse_char_of lower_case *~ parse_word *>~ fun (a, b) -> a ^ b

let parse_variable_string =
  parse_char_of upper_case
  +~ parse_char_of upper_case *~ parse_word *>~ fun (a, b) -> a ^ b

let counter_unnamed_ids = ref 0

let new_id () =
  counter_unnamed_ids := !counter_unnamed_ids + 1;
  Id ("_" ^ string_of_int !counter_unnamed_ids, 0)

let parse_generalvar =
  (parse_char '_' *>~ fun _ -> `GeneralVar (new_id ()))
  +~ parse_variable_string *>~ fun s -> `GeneralVar (Id (s, 0))

let rec table_of_termlist : table -> term list -> table =
 fun tail -> function
  | [] -> tail
  | h :: t -> `Table (h, table_of_termlist tail t)

let parenthesis p =
  parse_char '(' *~ p *~ parse_char ')' *>~ fun ((_, t), _) -> t

let bracket p = parse_char '[' *~ p *~ parse_char ']' *>~ fun ((_, t), _) -> t

let rec parse_term : term parser =
 fun cl -> cl |> parse_generalvar +~ parse_predicate +~ term_parsetable

and parse_predicate cl =
  cl
  |> ( parse_predicate_string *~ parenthesis parse_termlist *>~ fun (name, tl) ->
       `Predicate (name, tl) )
     +~ parse_predicate_string *>~ fun name -> `Predicate (name, [])

and parse_table : table parser =
 fun cl ->
  cl
  |> (bracket epsilon_parser *>~ fun _ -> `EmptyTable)
     +~ (bracket parse_termlist *>~ fun tl -> table_of_termlist `EmptyTable tl)
     +~ ( bracket (parse_termlist *~ parse_char '|' *~ parse_table)
        *>~ fun ((tl, _), tbl) -> table_of_termlist tbl tl )
     +~ bracket (parse_termlist *~ parse_char '|' *~ parse_variable_string)
        *>~ fun ((tl, _), var) -> table_of_termlist (`TableVar (Id (var, 0))) tl

and term_parsetable cl =
  parse_table cl |> List.map (fun (l, t) -> (l, (t :> term)))

and parse_termlist cl =
  cl
  |> (parse_term *>~ fun t -> [ t ])
     +~ parse_term *~ parse_char ',' *~ parse_termlist *>~ fun ((t, _), tl) ->
        t :: tl

let parse_clause =
  (parse_term *~ parse_char '.' *>~ fun (t, _) -> Clause (t, []))
  +~ parse_term *~ parse_char ':' *~ parse_char '-' *~ parse_termlist
     *~ parse_char '.'
     *>~ fun ((((t, _), _), tl), _) -> Clause (t, tl)

let rec parse_program : clause list parser =
 fun cl ->
  cl
  |> (parse_clause *>~ fun c -> [ c ])
     +~ parse_clause *~ parse_program *>~ fun (c, cl) -> c :: cl

let parse_request = parse_termlist *~ parse_char '?' *>~ fun (l, _) -> l

let unambiguous_parser_of parser s =
  let results =
    s |> charlist_of_string |> remove_spaces |> parser
    |> List.filter (fun (l, _) -> l = [])
  in
  match results with
  | [] -> failwith "Parsing failed, I don't understand what you're saying !"
  | [ (_, a) ] -> a
  | _ :: _ :: _ -> failwith "Parsing failed, my grammar in ambiguous !"

let term_of_string = unambiguous_parser_of parse_term

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
  | GeneralSubst (var, t) ->
      map_vars (function v -> if var = v then t else (v :> term)) id
  | TableSubst (var, t) ->
      map_vars id (function v -> if var = v then t else (v :> table))

(* De droite à gauche *)
let apply (s : substitution) term = List.fold_right apply_subst s term

(* ************************************
            Unification Algorithm
   ************************************ *)

let rec unify : term -> term -> substitution option =
 fun t1 t2 ->
  let f ta tb s1 =
    Option.bind (unify (apply s1 ta) (apply s1 tb)) (fun s2 -> Some (s2 @ s1))
  in
  match (t1, t2) with
  | (`TableVar _ as v), ((`TableVar _ | `EmptyTable | `Table _) as t) ->
      Some [ TableSubst (v, t) ]
  | (`EmptyTable | `Table _), `TableVar _ -> unify t2 t1
  | `EmptyTable, `EmptyTable -> Some []
  | `Table (h1, t1), `Table (h2, t2) ->
      Option.bind (unify h1 h2) (f (t1 :> term) (t2 :> term))
  | `Table _, `EmptyTable | `EmptyTable, `Table _ -> None
  | (`GeneralVar _ as v), _ -> Some [ GeneralSubst (v, t2) ]
  | _, `GeneralVar _ -> unify t2 t1
  | `Predicate (sa, la), `Predicate (sb, lb) ->
      if sa <> sb then None
      else if List.length la <> List.length lb then None
      else
        List.fold_left2
          (fun opt_s ta tb -> Option.bind opt_s (f ta tb))
          (Some []) la lb
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

let variables_in_clause (Clause (t, l)) =
  List.concat_map variables_in_term (t :: l)

let variables_in_request r = List.concat_map variables_in_term r

let replace_tvars_in_term tvars =
  map_vars
    (fun (`GeneralVar id) ->
      if List.mem (`TableVar id) tvars then `TableVar id else `GeneralVar id)
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

let _print_request r =
  "Request : " ^ (r |> List.map string_of_term_debug |> String.concat ", ")
  |> print_endline

(* La fonction de recherche, renvoie l'arbre des solutions *)
let rec sld_tree world request substitution n =
  (* print_request request; *)
  match request with
  | [] -> Leaf substitution
  | head_request_term :: other_request_terms ->
      let filter_clause c =
        let (Clause (left_member, right_member)) = rename n c in
        (*print_endline
          ("Will try to unify : "
          ^ string_of_term_debug head_request_term
          ^ " and "
          ^ string_of_term_debug left_member);*)
        let new_tree unifier =
          (* print_substitution unifier; *)
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
  | Node tl ->
      Seq.flat_map (fun par -> to_seq (Lazy.force par)) (list_to_seq tl)

let solutions tree vars =
  Seq.map
    (fun substitution -> (vars, List.map (apply substitution) vars))
    (to_seq tree)

(* ************************************
            Interface Functions
   ************************************ *)

let program_of_string s =
  s |> unambiguous_parser_of parse_program |> type_inference_program

let request_of_string s =
  s |> unambiguous_parser_of parse_request |> type_inference_request

let request program request =
  let termlist = request_of_string request in
  let vars =
    List.concat_map variables_in_term termlist |> List.sort_uniq Stdlib.compare
  in
  let world = program_of_string program in
  let sol = solutions (sld_tree world termlist [] 1) (vars :> term list) in
  let f (vars_tl, tl) =
    if vars = [] then Format.printf "This is true.\n%!"
    else
      List.map2
        (fun v t -> string_of_term v ^ " = " ^ string_of_term t)
        vars_tl tl
      |> String.concat ", "
      |> Format.printf "A solution is : %s\n%!"
  in
  if sol () = Seq.Nil then Format.printf "This is false.\n%!"
  else Seq.iter f sol;
  Format.printf "\n%!"

let _test_mgu t1 t2 =
  match unify (term_of_string t1) (term_of_string t2) with
  | None -> print_endline "No unification found"
  | Some substitution -> print_substitution substitution
