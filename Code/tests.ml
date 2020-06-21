open Types
open Utilitary
open Unification
open Solver


type 'a ftree = FLeaf of 'a | FNode of 'a ftree list

let rec force_tree (tree : 'a tree) =
  match tree with
  | Leaf a -> FLeaf a
  | Node atl -> FNode (List.map (fun t -> force_tree (Lazy.force t)) atl)

let test_eq s1 s2 =
  let _ = Format.printf "Bijection entre %s et %s :\n%!" s1 s2 in
  Option.iter
    (VarMap.iter (fun key v ->
         Format.printf "%s -> %s\n%!" (string_of_term (Var key)) (string_of_term (Var v))))
    (bijection (read_term s1) (read_term s2))


(* La fonction test_unification a pour but de tester mgu *)
let test_unification str_r str_c = Format.printf "Unification de %s et de %s :\n%!" str_r str_c;
match mgu (str_r |> read_term) (str_c |> read_term) with
| Some t -> VarMap.iter
    (fun (Id (s, _)) term -> Format.printf "%s <- %s\n%!" s (string_of_term term)) t
| None -> Format.printf "No unification\n%!"



let _ = test_unification "etudiant_de(E, pierre)" "etudiant_de(F,P)"; (* F/E et P/pierre *)
test_unification "etudiant_de(F,P)" "etudiant_de(E, pierre)"; (* E/F et P/pierre *)
test_unification "f(X,g(Y))" "f(g(Z),Z)"; (* X/g(g(Y)) et Z/g(Y) *)
test_unification "f(g(Z),Z)" "f(X,g(Y))"; (* X/g(g(Y)) et Z/g(Y) *)
test_unification "[a, a, b, c]" "[A | B]";
test_unification "[a, a, b, c]" "[A | A]";
test_unification "[[a, b, c], a, b, c]" "[A | A]"


let _ = test_eq "table(A, B)" "table(C, C)";
test_eq "table(A, A)" "table(B, C)";
test_eq "table(A, B)" "table(A, B)";
test_eq "[A, B, C, pinguin(AB, BA) | D]" "[Z, Y, X, pinguin(BA, AB) | W]";;

let world = read_program "
apprend(eve, mathematiques).
apprend(benjamin, informatique).
apprend(benjamin, physique).
enseigne(alice, physique).
enseigne(pierre, mathematiques).
enseigne(pierre, informatique).

etudiant_de(E,P):-apprend(E,M), enseigne(P,M).
" in let req = request world "standard" in
req "etudiant_de(E, pierre)";
req "etudiant_de(E, pierre), etudiant_de(E, alice)";
req "etudiant_de(A, B)";
req"etudiant_de(A, A)";
req "apprend(A, A)";
req "enseigne(A, A)";
req "enseigne(alice, physique)";
req "enseigne(alice, mathematiques)";;

let world = read_program "
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
in let req = request world "standard" in
req "connected(piccadilly_circus,leicester_square,piccadilly)";
req "nearby(oxford_circus, charing_cross)";
req "nearby(tottenham_court_road,W)";
req"reachable(bond_street, leicester_square)";
req"connected(oxford_circus, bond_street, L)";
req "path(oxford_circus, charing_cross, R)";;

let world = read_program "
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
" in let req = request world "standard" in

req "member(a, [c,d,a,b])";
req "prefix(P, [c,d,a,b])";
req "sublist(S, [a,b,c,d])";
req "append([a,b,c], [d,e,f], X)";
req "reverse([a,b,c,d,e,f], R)";
req "adjacent(X, Y, [a,b,c,d])";
req "equal([a, b], X)";;

let world = read_program "
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
" in let req = request world "standard" in
req "satisfiable(false)";;

let world = read_program "
natural(s(N)) :- natural(N).
natural(zero).
" in let req = request world "loops" in
req "natural(X)";;
(*
\begin{figure}
\centering
\includegraphics[scale=0.6]{london.png}
\caption{Métro londonien (piqué dans un bouquin)}
\end{figure}
*)
let world = read_program "
member(X, [X|Xs]).
member(X, [Y|Ys]) :- member(X, Ys).

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

connected(X, Y, L):-connected(Y, X, L).

nearby(X,Y):-connected(X,Y,L).
nearby(X,Y):-connected(X,Z,L),connected(Z,Y,L).

reachable(X,Y):-connected(X,Y,L).
reachable(X,Y):-connected(X,Z,L),reachable(Z,Y).

path(X,Y,[]) :- connected(X,Y,L).
path(X,Y, [Z|R]) :- connected(X,Z,L),path(Z,Y,R).

useful_path(X, Y, L) :- path(X, Y, L), naf(member(X, L)), naf(member(Y, L)).
" in let req = request world "neg" in
req "useful_path(oxford_circus, charing_cross, R)";;

let world = read_program "
natural(s(N)) :- natural(N).
natural(zero).
" in let req = request world "neg" in
req "naf(natural(s(s(zero2))))";;

let world = read_program "
member(X, [X|Xs]).
member(X, [Y|Ys]) :- member(X, Ys).

egal(X,X).
pas_egal(X,Y) :- naf(egal(X, Y)).

longueur([], zero).
longueur([X | L], s(K)) :- longueur(L, K).

remove(E, [], []).
remove(E, [E | Q], N) :- remove(E, Q, N).
remove(E, [T | Q], [T | N]) :- pas_egal(E, T), remove(E, Q, N).

difference([], S, []).
difference([E | Q], S, N) :- member(E, S), difference(Q, S, N).
difference([E | Q], S, [E | N]) :- naf(member(E, S)), difference(Q, S, N).


joueurs([samuel, pierre, benjamin]).
cartes([cuisine, chambre, salle_de_bain, salon, garage, salle_a_manger, veranda]).
cartes_par_joueur(s(s(zero))).


possede(samuel, cuisine).
possede(samuel, chambre).

possede_aucune_triplet(pierre, [salle_de_bain, salon, veranda]).
possede_une_des(pierre, [garage, cuisine, salon]).


possede(J, C) :- possede_une_des(J, L), member(C, L), remove(C, L, Ln), possede_aucune(J, Ln).

possede_pas(J, C) :- possede_aucune_triplet(J, L), member(C, L).
possede_pas(J1, C) :- possede(J2, C), pas_egal(J1, J2).

garde_si_possede(J, [], []).
garde_si_possede(J, [C | L], [C | Ln]) :- possede(J, C), garde_si_possede(J, L, Ln).
garde_si_possede(J, [C | L], Ln) :- naf(possede(J, C)), garde_si_possede(J, L, Ln).
possede_liste_unique(J, L) :- cartes(Cl), garde_si_possede(J, Cl, L).

possedent([J | Jl], C) :- possede(J, C).
possedent([J | Jl], C) :- possedent(Jl, C).
qqn_possede(C) :- joueurs(Lj), possedent(Lj, C).

garde_si_qqn_possede([], []).
garde_si_qqn_possede([C | L], [C | Ln]) :- qqn_possede(C), garde_si_qqn_possede(L, Ln).
garde_si_qqn_possede([C | L], Ln) :- naf(qqn_possede(C)), garde_si_qqn_possede(L, Ln).
possedees_par_qqn(L) :- cartes(Cl), garde_si_qqn_possede(Cl, L).

possedent_pas([], C).
possedent_pas([J | Jl], C) :- possede_pas(J, C), possedent_pas(Jl, C).
personne_ne_possede(C) :- joueurs(Lj), possedent_pas(Lj, C).

possede_aucune(J, []).
possede_aucune(J, [C | T]) :- possede_pas(J, C), possede_aucune(J, T).

strange(J, C) :-
    possede_liste_unique(J, L),
    cartes_par_joueur(N),
    longueur(L, N),
    cartes(Cl),
    difference(Cl, L, Lbar),
    member(C, Lbar).
"
in let req = request world "neg" in
req "possede(pierre, garage)";
req "strange(samuel, cuisine)";
req "possede_liste_unique(samuel, L)";
req "possedees_par_qqn(L)";
req "difference([a, b, c, c, d], [b, c], L)";
req "longueur([a, b, c], K)";;

let world = read_program "
h(sam).
h(sam).
test(X) :- h(sam)." in
let req = request world "neg" in
req "test(X)";;
