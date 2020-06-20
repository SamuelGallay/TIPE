open Types
open Utilitary


(* E est un ensemble couples de termes *)
(* transforme E en un équivalent sous forme résolue (type option, None si impossible) *)
type equation_set = {mutable data : TermSet.t; mutable correct : bool}

let solve e =
let eqs = {data = e; correct = true} in

let aux = function
| Predicate (f, lf), Predicate (g, lg) when f = g && List.length lf = List.length lg ->
    List.iter2 (fun a b -> eqs.data <- TermSet.add (a,b) eqs.data) lf lg;
    eqs.data <- TermSet.remove (Predicate (f, lf), Predicate (g, lg)) eqs.data;

| Predicate (_, _), Predicate (_, _) ->
    eqs.correct <- false;

| Var x, Var y when x = y ->
    eqs.data <- TermSet.remove (Var x, Var y) eqs.data

| Predicate (a, la), Var x ->
    eqs.data <- TermSet.remove (Predicate (a, la), Var x) eqs.data;
    eqs.data <- TermSet.add    (Var x, Predicate (a, la)) eqs.data;

| Table t, Var x ->
    eqs.data <- TermSet.remove (Table t, Var x) eqs.data;
    eqs.data <- TermSet.add    (Var x, Table t) eqs.data;

| Var w, t when var_in_eqs w (TermSet.remove (Var w, t) eqs.data) ->
    if var_in_term w t then eqs.correct <- false
    else (
    eqs.data <- TermSet.remove (Var w, t) eqs.data;
    eqs.data <- replace_var_in_eqs w t eqs.data;
    eqs.data <- TermSet.add (Var w, t) eqs.data;
    )

| Var _, _ ->
    ()

| Predicate (_, _), Table _ ->
    eqs.correct <- false

| Table _, Predicate (_, _) ->
    eqs.correct <- false

| Table t1, Table t2 -> (
    match t1, t2 with
    | Empty, Empty ->
        eqs.data <- TermSet.remove (Table Empty, Table Empty) eqs.data;

    | Empty, NonEmpty _ ->
        eqs.correct <- false;

    | NonEmpty _, Empty ->
        eqs.correct <- false;

    | (NonEmpty (head1, tail1) as t1), (NonEmpty (head2, tail2) as t2) ->
        eqs.data <- TermSet.remove (Table t1, Table t2) eqs.data;
        eqs.data <- TermSet.add (head1, head2) eqs.data;
        eqs.data <- TermSet.add (Table tail1, Table tail2) eqs.data;

    | (NonEmpty (_, _) | Empty as t), TVar a ->
        eqs.data <- TermSet.remove (Table t, Table (TVar a)) eqs.data;
        eqs.data <- TermSet.add (Table (TVar a), Table t) eqs.data;

    | TVar a, TVar b when a=b ->
        eqs.data <- TermSet.remove (Table (TVar a), Table (TVar b)) eqs.data;

    | TVar w, table when var_in_eqs w (TermSet.remove (Table (TVar w), Table table) eqs.data) ->
        if var_in_term w (Table table) then eqs.correct <- false
        else eqs.data <- replace_var_in_eqs w (Table table) eqs.data;

    | TVar _ , _ ->
        ()
    )
in
let old = ref TermSet.empty in
while eqs.data <> !old && eqs.correct do
old := eqs.data;
TermSet.iter (fun c -> if eqs.data <> !old then () else aux c) eqs.data;
done;

if eqs.correct then Some eqs.data else None

(* Prend 2 terme et renvoie l'unifieur le plus général s'il existe, None sinon.
Attention, aucune variable ne doit être présente dans
(r) la requête et dans (c) la tête de la clause *)

let mgu r c = let f = function
    | Var x, t -> x, t
    | Predicate (_, _), _ -> failwith "Should not happen."
    | Table (TVar r), t -> r, t
    | Table (Empty | NonEmpty _), _ -> failwith "Should not happen."
in match solve (TermSet.singleton (r, c)) with
| None -> None
| Some e -> Some (e |> TermSet.to_seq |> Seq.map f |> VarMap.of_seq);;
