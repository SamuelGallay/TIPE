open Types

let rec rename_var old_var new_var = function
  | Var v -> Var (if v = old_var then new_var else v)
  | Predicate (p, l) -> Predicate (p, List.map (rename_var old_var new_var) l)
  | Table tbl ->
      Table
        ( match tbl with
        | TVar v -> TVar (if v = old_var then new_var else v)
        | Empty -> Empty
        | NonEmpty (head, tail) -> (
            match rename_var old_var new_var (Table tail) with
            | Table ntail -> NonEmpty (rename_var old_var new_var head, ntail)
            | _ -> failwith "Error in rename var" ) )

let rec list_var_in_term = function
  | Var v -> [ v ]
  | Predicate (_, l) -> List.concat (List.map list_var_in_term l)
  | Table tbl -> (
      match tbl with
      | Empty -> []
      | TVar v -> [ v ]
      | NonEmpty (h, t) -> list_var_in_term h @ list_var_in_term (Table t) )

let normalize term =
  let nt, _ =
    List.fold_left
      (fun (t, n) v -> (rename_var v (Id ("@", n)) t, n + 1))
      (term, 1) (list_var_in_term term)
  in
  nt

module type NormalTermSet_type = sig
  type t

  val empty : t

  val add : term -> t -> t

  val mem : term -> t -> bool
end

module NormalTermSet : NormalTermSet_type = struct
  module M = Set.Make (struct
    type t = term

    let compare = compare
  end)

  type t = M.t

  let empty = M.empty

  let add e = e |> normalize |> M.add

  let mem e = e |> normalize |> M.mem
end
