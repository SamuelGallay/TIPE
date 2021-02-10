open Types
open Utilitary
open Unification
open Solver

type 'a ftree = FLeaf of 'a | FNode of 'a ftree list

let rec force_tree (tree : 'a tree) =
  match tree with
  | Leaf a -> FLeaf a
  | Node atl -> FNode (List.map (fun t -> force_tree (Lazy.force t)) atl)

(* La fonction test_unification a pour but de tester mgu *)
let test_unification str_r str_c =
  Format.printf "Unification de %s et de %s :\n%!" str_r str_c;
  match mgu (str_r |> read_term) (str_c |> read_term) with
  | Some t ->
      VarMap.iter (fun (Id (s, _)) term -> Format.printf "%s <- %s\n%!" s (string_of_term term)) t
  | None -> Format.printf "No unification\n%!"

let test1 () =
  test_unification "etudiant_de(E, pierre)" "etudiant_de(F,P)";
  (* F/E et P/pierre *)
  test_unification "etudiant_de(F,P)" "etudiant_de(E, pierre)";
  (* E/F et P/pierre *)
  test_unification "f(X,g(Y))" "f(g(Z),Z)";
  (* X/g(g(Y)) et Z/g(Y) *)
  test_unification "f(g(Z),Z)" "f(X,g(Y))";
  (* X/g(g(Y)) et Z/g(Y) *)
  test_unification "[a, a, b, c]" "[A | B]";
  test_unification "[a, a, b, c]" "[A | A]";
  test_unification "[[a, b, c], a, b, c]" "[A | A]"

let test3 () =
  let world =
    read_program
    "
apprend(eve, mathematiques).
apprend(benjamin, informatique).
apprend(benjamin, physique).
enseigne(alice, physique).
enseigne(pierre, mathematiques).
enseigne(pierre, informatique).

etudiant_de(E,P):-apprend(E,M), enseigne(P,M).
"
  in
  let req = request world "standard" in
  req "etudiant_de(E, pierre)";
  req "etudiant_de(E, pierre), etudiant_de(E, alice)";
  req "etudiant_de(A, B)";
  req "etudiant_de(A, A)";
  req "apprend(A, A)";
  req "enseigne(A, A)";
  req "enseigne(alice, physique)";
  req "enseigne(alice, mathematiques)"

let test4 () =
  let world =
    read_program
    "
connected(bond_street,oxford_circus,central).
connected(oxford_circus,tottenham_court_road,central).
connected(bond_street,green_park,jubilee).
connected(green_park,charing_cross,jubilee).
connected(green_park,piccadilly_circus,piccadilly).
connected(piccadilly_circus,leicester_square,piccadilly).
connected(green_park,oxford_circus,victoria).
connected(oxford_circus,piccadilly_circus,bakerloo).
connected(piccadilly_circus,charing_cross,bakerloo).
connected(tottenham_court_road,leicester_square,northern).
connected(leicester_square,charing_cross,northern).

nearby(X,Y):-connected(X,Y,L).
nearby(X,Y):-connected(X,Z,L),connected(Z,Y,L).

reachable(X,Y):-connected(X,Y,L).
reachable(X,Y):-connected(X,Z,L),reachable(Z,Y).

path(X,Y,noroute):-connected(X,Y,L).
path(X,Y,route(Z,R)):-connected(X,Z,L),path(Z,Y,R)."
  in
  let req = request world "standard" in
  req "connected(piccadilly_circus,leicester_square,piccadilly)";
  req "nearby(oxford_circus, charing_cross)";
  req "nearby(tottenham_court_road,W)";
  req "reachable(bond_street, leicester_square)";
  req "connected(oxford_circus, bond_street, L)";
  req "path(oxford_circus, charing_cross, R)"

;;

let test5 () =
  let world =
    read_program
      "
member(X, [X|Xs]).
member(X, [Y|Ys]) :- member(X, Ys).

prefix([], Ys).
prefix([X|Xs],[X|Ys]) :- prefix(Xs, Ys).

sublist(Xs, Ys) :- prefix(Xs, Ys).
sublist(Xs, [Y|Ys]) :- sublist (Xs, Ys).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

reverse([],[]).
reverse([X|Xs], R) :- reverse(Xs, Z), append(Z, [X], R).

adjacent(X,Y,Zs) :- append(As, [X,Y|Ys] ,Zs).

equal(X, X).
"
  in
  let req = request world "standard" in

  req "member(a, [c,d,a,b])";
  req "prefix(P, [c,d,a,b])";
  req "sublist(S, [a,b,c,d])";
  req "append([a,b,c], [d,e,f], X)";
  req "reverse([a,b,c,d,e,f], R)";
  req "adjacent(X, Y, [a,b,c,d])";
  req "equal([a, b], X)"

;;
let test6 () =
  let world =
    read_program
      "
satisfiable(true).
satisfiable(and(X, Y)) :- satisfiable(X), satisfiable(Y).
satisfiable(or(X, Y)) :- satisfiable(X).
satisfiable(or(X, Y)) :- satisfiable(Y).
satisfiable(not(X)) :- invalid(X).

invalid(false).
invalid(or(X, Y)) :- invalid(X), invalid(Y).
invalid(and(X, Y)) :- invalid(X).
invalid(and(X, Y)) :- invalid(Y).
invalid(not(X)) :- satisfiable(X).
"
  in
  let req = request world "standard" in
  req "and(true, true)"



(* nationality pet cigarette drink house-color *)
let zebra () =
  let req =
  request
    (read_program
"
member(X, [X | Xs]).
member(X, [Y | Ys]) :- member(X, Ys).

isright(L, R, [L, R | T]).
isright(L, R, [H | T]) :- isright(L, R, T).

nextto(A, B, X) :- isright(A, B, X).
nextto(A, B, X) :- isright(B, A, X).

equal(X, X).

zebra(H, W, Z):-
equal(H, [[norwegian, _, _, _, _], _, [_, _, _, milk, _], _, _]),
member([englishman, _, _, _, red], H),
member([spaniard, dog, _, _, _], H),
member([_, _, _, coffee, green], H),
member([ukrainian, _, _, tea, _], H),
member([_, snails, winston, _, _], H),
member([_, _, kools, _, yellow], H),
member([_, _, luckystrike, orangejuice, _], H),
member([japanese, _, parliaments, _, _], H),
nextto([norwegian, _, _, _, _], [_, _, _, _, blue], H),
isright([_, _, _, _, ivory], [_, _, _, _, green], H),
nextto([_, _, chesterfields, _, _], [_, fox, _, _, _], H),
nextto([_, _, kools, _, _], [_, horse, _, _, _], H),
member([W, _, _, water, _], H),
member([Z, zebra, _, _, _], H).
")
    "standard"
in
req "zebra(Houses, WaterDrinker, ZebraOwner)"
