let () =
  Format.printf "Bienvenue !\n\n";

  Tests.zebra ();

  let world =
    Solver.read_program
      "
sous_sujet(alice, groupe1).
sous_sujet(groupe1, groupe2).
sous_sujet(bob, groupe2).

sous_objet(fichierA, dossier1).
sous_objet(fichierB, dossier2).
sous_objet(dossier1, dossier2).

autorise(groupe1, dossier2).

autorise(U, F) :-  sous_objet(F, D), autorise(U, D).
autorise(U, F) :-  sous_sujet(U, G), autorise(G, F).
"
  in

  let req = Solver.request world "standard" in

  req "autorise(U, F) ?"
