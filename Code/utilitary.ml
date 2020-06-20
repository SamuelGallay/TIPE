open Types

(* Transforme un terme en une chaîne de caractères *)
let rec string_of_term = function
  | Predicate (p, []) -> p
  | Predicate (p, l) ->
      p ^ "(" ^ String.concat ", " (List.map string_of_term l) ^ ")"
  | Var (Id (s, n)) -> s ^ string_of_int n
  | Table Empty -> "[]"
  | Table (TVar (Id (s, n))) -> s ^ string_of_int n
  | Table (NonEmpty (t, l)) ->
      let rec aux = function
        | Empty -> ""
        | TVar (Id (s, n)) -> "|" ^ s ^ string_of_int n
        | NonEmpty (t, l) -> ", " ^ string_of_term t ^ aux l
      in
      "[" ^ string_of_term t ^ aux l ^ "]"

(* DEBUG : affiche un liste de couple de termes *)
let print_eqs eqs =
  TermSet.iter
    (fun (t1, t2) ->
      Format.printf "%s <-> %s\n%!" (string_of_term t1) (string_of_term t2))
    eqs

(* Applique une substitution sur un terme *)
(* Attention, on ne peut remplacer une variable qui représente un tableau que par un tableau *)
let rec replace_var_in_term var new_term term =
  match term with
  | Var v -> if v = var then new_term else Var v
  | Predicate (p, l) ->
      Predicate (p, List.map (replace_var_in_term var new_term) l)
  | Table tbl ->
      let rec aux = function
        | Empty -> Empty
        | TVar v ->
            if v = var then
              match new_term with
              | Table nt -> nt
              | _ ->
                  failwith
                    "Try to replace a table by a term that is not a table"
            else TVar v
        | NonEmpty (head, tail) ->
            NonEmpty (replace_var_in_term var new_term head, aux tail)
      in
      Table (aux tbl)

let replace_var_in_eqs var new_term eqs =
  TermSet.map
    (fun (a, b) ->
      (replace_var_in_term var new_term a, replace_var_in_term var new_term b))
    eqs

let rec var_in_term var = function
  | Var v -> if v = var then true else false
  | Predicate (_, l) -> List.mem true (List.map (var_in_term var) l)
  | Table t ->
      let rec aux = function
        | Empty -> false
        | NonEmpty (head, tail) -> var_in_term var head || aux tail
        | TVar v -> if v = var then true else false
      in
      aux t

let var_in_eqs var eqs =
  TermSet.exists (function a, b -> var_in_term var a || var_in_term var b) eqs

(* Recherche les termes qui sont des variables récursivement dans une liste de termes *)
let rec find_vars_in_termlist tl =
  let rec find_vars_in_term = function
    | Var v -> [ Var v ]
    | Predicate (_, l) -> find_vars_in_termlist l
    | Table t ->
        let rec aux = function
          | Empty -> []
          | TVar v -> [ Table (TVar v) ]
          | NonEmpty (head, tail) -> find_vars_in_term head @ aux tail
        in
        aux t
  in
  List.sort_uniq compare (List.concat (List.map find_vars_in_term tl))

(* Renvoie la liste des variables représentant un tableau à l'intérieur d'un terme *)
let rec find_tvars_in_term = function
  | Var _ -> []
  | Predicate (_, l) -> List.concat (List.map find_tvars_in_term l)
  | Table t ->
      let rec aux = function
        | Empty -> []
        | NonEmpty (head, tail) -> find_tvars_in_term head @ aux tail
        | TVar v -> [ v ]
      in
      aux t

(* Renvoie la liste des variables représentant un tableau à l'intérieur d'une liste de termes *)
let find_tvars_in_termlist tl = List.concat (List.map find_tvars_in_term tl)

(* Renvoie la liste des variables représentant un tableau à l'intérieur d'une clause *)
let find_tvars_in_clause (Clause (t, tl)) =
  find_tvars_in_term t @ find_tvars_in_termlist tl
