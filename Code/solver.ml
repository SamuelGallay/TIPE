open Types
open Utilitary
open Unification

let type_check_term term =
  List.fold_left
    (fun t v -> replace_var_in_term v (Table (TVar v)) t)
    term (find_tvars_in_term term)

let type_check_cl cl =
  let l = find_tvars_in_clause cl in
  List.fold_left
    (fun cl v ->
      let aux = replace_var_in_term v (Table (TVar v)) and (Clause (left, right)) = cl in
      Clause (aux left, List.map aux right))
    cl l

let read_program str =
  List.map type_check_cl (str |> Lexing.from_string |> Parser.programme Lexer.mylexer)

let read_request str =
  str |> Lexing.from_string |> Parser.requete Lexer.mylexer |> List.map type_check_term

(* Applique une substitution *)
let apply_subst_on_term = VarMap.fold replace_var_in_term

(* Applique une substitution sur une liste de termes *)
let apply_subst_on_termlist uni = apply_subst_on_term uni |> List.map

(* Prend un entier et une clause, renvoie la clause avec les variables renommées à l'entier *)
let rename n (Clause (t1, tl)) =
  let rec f = function
    | Var (Id (str, _)) -> Var (Id (str, n))
    | Predicate (atm, l) -> Predicate (atm, List.map f l)
    | Table t ->
        let rec aux = function
          | Empty -> Empty
          | TVar (Id (str, _)) -> TVar (Id (str, n))
          | NonEmpty (head, tail) -> NonEmpty (f head, aux tail)
        in
        Table (aux t)
  in
  Clause (f t1, List.map f tl)

let list_to_seq l = List.fold_right (fun x s () -> Seq.Cons (x, s)) l Seq.empty

let rec to_seq = function
  | Leaf str -> Seq.return str
  | Node tl -> Seq.flat_map (fun par -> to_seq (Lazy.force par)) (list_to_seq tl)

(* La fonction de recherche, renvoie l'arbre des solutions *)
let rec sld_tree world req subs n =
  match req with
  | [] -> Leaf subs
  | head_request_term :: other_request_terms ->
      Node
        (List.filter_map
           (fun c ->
             let (Clause (left_member, right_member)) = rename n c in
             match mgu head_request_term left_member with
             | None -> None
             | Some unifier ->
                 Some
                   (lazy
                     (sld_tree world
                        (apply_subst_on_termlist unifier (right_member @ other_request_terms))
                        (unifier :: subs) (n + 1))))
           world)

let solutions tree vars =
  Seq.map (fun l -> (vars, List.fold_right apply_subst_on_termlist l vars)) (to_seq tree)

let request world sol_method req =
  let termlist = read_request req in
  let vars = find_vars_in_termlist termlist in
  let sol =
    match sol_method with
    | "standard" -> solutions (sld_tree world termlist [] 1) vars
    | _ -> failwith "Unknown solution method"
  in
  if sol () = Seq.Nil then Format.printf "This is false.\n%!"
  else
    Seq.iter
      (fun (vars, tl) ->
        if vars = [] then Format.printf "This is true.\n%!"
        else
          Format.printf "There is : %s\n%!"
            (String.concat ", "
               (List.map2 (fun v t -> string_of_term v ^ " = " ^ string_of_term t) vars tl)))
      sol;
  Format.printf "\n%!"
