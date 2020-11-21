open Types

type structure =
  | S of string
  | V of var
  | L of table
  | T of term
  | C of clause
  | W of clause list
  | P of string
  | TL of term list

let to_list str = List.init (String.length str) (String.get str)

let rec remove_spaces = function
  | [] -> []
  | (' ' | '\t' | '\n') :: l -> remove_spaces l
  | a :: l -> a :: remove_spaces l

let of_char c = String.make 1 c

let ( <+> ) ra rb l = ra l @ rb l

let chain y l = List.map (fun (a, b) -> (a, y @ b)) l

let ( *~* ) ra rb l = List.concat (List.map (fun (x, y) -> chain y (rb x)) (ra l))

let ( >~> ) ra f l = List.map (fun (a, b) -> (a, f b)) (ra l)

let skip _ = []

let fix aux =
  let rec g x = aux g x in
  g

let rlow = function ('a' .. 'z' as c) :: l -> [ (l, [ S (of_char c) ]) ] | _ -> []

let rup = function ('A' .. 'Z' as c) :: l -> [ (l, [ S (of_char c) ]) ] | _ -> []

let rsym s = function h :: t when s = h -> [ (t, [ S (of_char s) ]) ] | _ -> []

let rchar = function
  | (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c) :: t -> [ (t, [ S (of_char c) ]) ]
  | _ -> []

let rmot =
  let aux self = rchar <+> rchar *~* self >~> function [ S s; S s' ] -> [ S (s ^ s') ] | l -> l in
  fix aux

let iter = ref 0

let newvar () =
  iter := !iter + 1;
  V (Id ("$" ^ string_of_int !iter, 0))

let variable =
  rup
  >~> (function [ S s ] -> [ V (Id (s, 0)) ] | l -> l)
  <+> rsym '_'
  >~> (function [ S _ ] -> [ newvar () ] | l -> l)
  <+> rup *~* rmot
  >~> function
  | [ S s; S s' ] -> [ V (Id (s ^ s', 0)) ]
  | l -> l

let predicate =
  rlow >~> (function [ S s ] -> [ P s ] | l -> l) <+> rlow *~* rmot >~> function
  | [ S s; S s' ] -> [ P (s ^ s') ]
  | l -> l

let rec table_from_list tl t = match tl with h :: q -> NonEmpty (h, table_from_list q t) | [] -> t

let rec term l =
  (variable
   >~> (function [ V s ] -> [ T (Var s) ] | l -> l)
   <+> predicate
   >~> (function [ P s ] -> [ T (Predicate (s, [])) ] | l -> l)
   <+> predicate *~* (rsym '(' >~> skip) *~* termlist *~* (rsym ')' >~> skip)
   >~> (function [ P s; TL tl ] -> [ T (Predicate (s, tl)) ] | l -> l)
   <+> table
   >~> function
   | [ L t ] -> [ T (Table t) ]
   | l -> l)
    l

and termlist l =
  (term
   >~> (function [ T t ] -> [ TL [ t ] ] | l -> l)
   <+> term *~* (rsym ',' >~> skip) *~* termlist
   >~> function
   | [ T t; TL tl ] -> [ TL (t :: tl) ]
   | l -> l)
    l

and table l =
  (rsym '[' *~* rsym ']'
   >~> (function [ S "["; S "]" ] -> [ L Empty ] | l -> l)
   <+> (rsym '[' >~> skip) *~* termlist *~* (rsym ']' >~> skip)
   >~> (function [ TL tl ] -> [ L (table_from_list tl Empty) ] | l -> l)
   <+> (rsym '[' >~> skip) *~* termlist *~* (rsym '|' >~> skip) *~* table *~* (rsym ']' >~> skip)
   >~> (function [ TL tl; L t ] -> [ L (table_from_list tl t) ] | l -> l)
   <+> (rsym '[' >~> skip) *~* termlist *~* (rsym '|' >~> skip) *~* variable *~* (rsym ']' >~> skip)
   >~> function
   | [ TL tl; V v ] -> [ L (table_from_list tl (TVar v)) ]
   | l -> l)
    l

let clause =
  term *~* (rsym '.' >~> skip)
  >~> (function [ T t ] -> [ C (Clause (t, [])) ] | l -> l)
  <+> term *~* (rsym ':' *~* rsym '-' >~> skip) *~* termlist *~* (rsym '.' >~> skip)
  >~> function
  | [ T t; TL tl ] -> [ C (Clause (t, tl)) ]
  | l -> l

let programme =
  let aux self =
    clause >~> (function [ C c ] -> [ W [ c ] ] | l -> l) <+> clause *~* self >~> function
    | [ C c; W cl ] -> [ W (c :: cl) ]
    | l -> l
  in
  fix aux

let check r l =
  let aux = function [], result -> Some result | _ -> None in
  match r l |> List.filter_map aux with
  | [] -> failwith "Parsing failed"
  | [ a ] -> a
  | _ -> failwith "Ambiguous grammar"

let parse_program s =
  match s |> to_list |> remove_spaces |> check programme with
  | [ W w ] -> w
  | _ -> failwith "input is not a program"

let parse_term s =
  match s |> to_list |> remove_spaces |> check term with
  | [ T t ] -> t
  | _ -> failwith "input is not a term"

let parse_clause c =
  match c |> to_list |> remove_spaces |> check clause with
  | [ C a ] -> a
  | _ -> failwith "input is not a clause"

let parse_termlist str =
  match str |> to_list |> remove_spaces |> check termlist with
  | [ TL tl ] -> tl
  | _ -> failwith "input is not a termlist"
