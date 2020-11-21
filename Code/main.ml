let _ = Format.printf "Bienvenue !\n\n";;

let () = Tests.zebra ();;

let world =
  Solver.read_program
"
possede(alice, fichierA).
possede(bob, fichierB).

group(users, [alice, bob]).
group(admins, [charlie, eve]).

autorise(alice, bob).

peutacceder(B, F) :- possede(B, F).
peutacceder(B, F) :- possede(A, F), autorise(A, B).

"
in
let req = Solver.request world "standard" in
req "peutacceder(bob, fichierB)";
req "peutacceder(bob, fichierA)";
req "peutacceder(alice, fichierB)";;


let () = Tests.test6 ();;
