open Types
open Parser
open Utilitary
open Unification

let type_check_term term =
List.fold_left (fun t v -> replace_var_in_term (v) (Table (TVar v)) t) term (find_tvars_in_term term)

let type_check_cl cl =
let l = find_tvars_in_clause cl in
List.fold_left (fun cl v ->
let aux = replace_var_in_term (v) (Table (TVar v))
and Clause (left, right) = cl in
Clause (aux left, List.map aux right)
) cl l

let read_term str = type_check_term (parse_term str)
let read_program str = List.map type_check_cl (parse_program str)
let read_clause str = type_check_cl (parse_clause str)
let read_termlist str = List.map type_check_term (parse_termlist str)

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
in Table (aux t)
in Clause (f t1, List.map f tl)

(* Renvoie un set de (Var Model -> Var Request) *)
let equivalent r m =
let rec equal map_opt request model = match map_opt with
| None -> None
| Some map ->
match request, model with
| Var vr, Var vm -> (match VarMap.find_opt vm map with
    | None -> Some (VarMap.add vm vr map)
    | Some vs -> if vs = vr then Some map else None)
| Predicate (pr, lr), Predicate (pm, lm) ->
    if pr <> pm || List.length lr <> List.length lm then None
    else List.fold_left2 equal (Some map) lr lm
| Table tr, Table tm ->
    let rec aux map_opt tbl_r tbl_m = match map_opt with
    | None -> None
    | Some map -> (
    match tbl_r, tbl_m with
    | Empty, Empty -> Some map
    | TVar vr, TVar vm -> (match VarMap.find_opt vm map with
        | None -> Some (VarMap.add vm vr map)
        | Some vs -> if vs = vr then Some map else None)
    | NonEmpty (head_r, tail_r), NonEmpty (head_m, tail_m) ->
        aux (equal (Some map) head_r head_m) tail_r tail_m
    | _, _ -> None
    ) in aux (Some map) tr tm
| _, _ -> None
in equal (Some (VarMap.empty)) r m

(* requête tête ->  (var tête -> var requête) *)
let bijection r m = match equivalent m r with
| None -> None
| Some map -> VarMap.fold
(fun key v b -> match b with
| None -> None
| Some nmap -> if VarMap.mem v nmap then None else Some (VarMap.add v key nmap))
map (Some VarMap.empty)

let exist_in term gen = List.exists
(fun t -> Option.is_some (bijection term t)) gen

type 'a tree = Leaf of 'a | Node of (('a tree) Lazy.t) list

let list_to_seq l = List.fold_right (fun x s -> (fun ()->Seq.Cons(x, s)) ) l Seq.empty

let rec to_seq = function
| Leaf str -> Seq.return str
| Node tl -> Seq.flat_map (fun par -> to_seq (Lazy.force par)) (list_to_seq tl)

(* La fonction de recherche, renvoie l'arbre des solutions *)
let rec sld_tree world req subs n = match req with
| [] -> Leaf subs
| head_request_term::other_request_terms ->
    Node (List.filter_map (fun c -> let Clause (left_member, right_member) = rename n c in
    (match mgu head_request_term left_member with
        | None -> None
        | Some unifier -> Some (lazy (sld_tree world
            (apply_subst_on_termlist unifier (right_member@other_request_terms))
            (unifier::subs) (n+1)))
    )) world)

let rec sld_tree_gen world req subs n = match req with
| [] -> Leaf subs
| (head_request_term, hr_gen)::other_request_terms ->
    if exist_in head_request_term hr_gen then Node [] else
    Node (List.filter_map (fun c -> let Clause (left_member, right_member) = rename n c in
    (match mgu head_request_term left_member with
        | None -> None
        | Some unifier -> Some (lazy (sld_tree_gen world
            (
            (List.map (fun t -> apply_subst_on_term unifier t, head_request_term::hr_gen) right_member)
            @ (List.map (fun (t, gen) -> apply_subst_on_term unifier t, gen) other_request_terms)
            )
        (unifier::subs) (n+1)))
    )) world)

(*Debug !*)
let iter = ref 0
let print_req req = iter := !iter + 1; if !iter >= 1000 then (iter := 0; failwith "Max Debug Stack") else
Format.printf "%s\n%!" (String.concat "," (List.map (fun (t, _) -> string_of_term t) req))

let debug_flag = ref false

let rec sld_tree_neg_gen world (req : (term * term list) list) subs n =
if !debug_flag then print_req req else ();
match req with
| [] -> Leaf subs
| (head_request_term, hr_gen)::other_request_terms ->
    if exist_in head_request_term hr_gen then Node [] else
    match head_request_term with
    | Predicate ("naf", tl) -> if naf world (List.map (fun t -> t, []) tl)
        then sld_tree_neg_gen world other_request_terms subs n
        else Node []
    | _ ->
    Node (List.filter_map (fun c -> let Clause (left_member, right_member) = rename n c in
    (match mgu head_request_term left_member with
        | None -> None
        | Some unifier -> Some (lazy (sld_tree_neg_gen world
            (
            (List.map (fun t -> apply_subst_on_term unifier t, head_request_term::hr_gen) right_member)
            @ (List.map (fun (t, gen) -> apply_subst_on_term unifier t, gen) other_request_terms)
            )
        (unifier::subs) (n+1)))
    )) world)
and naf world (req : (term * term list) list) =
let sols = to_seq (sld_tree_neg_gen world req [] 1) in
if sols () = Seq.Nil then true else false

let solutions tree vars =
Seq.map  (fun l -> vars,
List.fold_right apply_subst_on_termlist l vars
) (to_seq tree)

let request world sol_method req =
let termlist = read_termlist req in
let vars = find_vars_in_termlist termlist in
let sol = match sol_method with
| "standard" -> solutions (sld_tree world termlist [] 1) vars
| "loops" -> solutions (sld_tree_gen world (termlist |> List.map (fun t -> t, [])) [] 1) vars
| "neg" -> debug_flag := false; solutions (sld_tree_neg_gen world (termlist |> List.map (fun t -> t, [])) [] 1) vars
| "debug" -> debug_flag := true; solutions (sld_tree_neg_gen world (termlist |> List.map (fun t -> t, [])) [] 1) vars
| _ -> failwith "Unknown solution method"
in
if sol () = Seq.Nil then Format.printf "This is false.\n%!"
else Seq.iter (fun (vars, tl) ->
if vars = [] then Format.printf "This is true.\n%!"
else Format.printf "There is : %s\n%!"
(String.concat ", " (List.map2 (fun v t -> (string_of_term v) ^ " = " ^ (string_of_term t)) vars tl))
) sol;
Format.printf "\n%!"

let rec rename_var old_var new_var = function
| Var v -> Var (if v = old_var then new_var else v)
| Predicate (p, l) -> Predicate (p, List.map (rename_var old_var new_var) l)
| Table tbl -> Table (match tbl with
    | TVar v -> (TVar (if v = old_var then new_var else v))
    | Empty -> Empty
    | NonEmpty (head, tail) ->
        (match rename_var old_var new_var (Table tail) with
            | Table ntail -> NonEmpty (rename_var old_var new_var head, ntail)
            | _ -> failwith "Error in rename var")
    )

let rec list_var_in_term = function
| Var v -> [v]
| Predicate (_, l) -> List.concat (List.map list_var_in_term l)
| Table tbl -> match tbl with
    | Empty -> []
    | TVar v -> [v]
    | NonEmpty (h, t) -> (list_var_in_term h) @ list_var_in_term (Table t)

let normalize term = let (nt, _) =
List.fold_left (fun (t, n) v -> (rename_var v (Id ("@", n)) t), n+1) (term, 1) (list_var_in_term term)
in nt

let _ ="samuel(S, G, [S, A | S])" |> read_term |> normalize |> string_of_term |> Format.printf "%s\n%!"

type 'a ftree = FLeaf of 'a | FNode of ('a ftree) list

let rec force_tree (tree : 'a tree) = match tree with
| Leaf a -> FLeaf a
| Node atl -> FNode (List.map (fun t -> force_tree (Lazy.force t)) atl)

let test_eq s1 s2 =
  let _ = Format.printf "Bijection entre %s et %s :\n%!" s1 s2 in
  Option.iter (VarMap.iter (fun key v -> Format.printf "%s -> %s\n%!" (string_of_term (Var key)) (string_of_term (Var v))))
(bijection (read_term s1) (read_term s2))
