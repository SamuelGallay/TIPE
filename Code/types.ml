type var = Id of string * int

type table = Empty | NonEmpty of term * table | TVar of var

and term = Var of var | Predicate of string * term list | Table of table

type clause = Clause of term * term list
type 'a tree = Leaf of 'a | Node of 'a tree Lazy.t list

module TermSet = Set.Make (struct
  type t = term * term

  let compare = compare
end)

module VarMap = Map.Make (struct
  type t = var

  let compare = compare
end)

module TermMap = Map.Make (struct
  type t = term

  let compare = compare
end)
