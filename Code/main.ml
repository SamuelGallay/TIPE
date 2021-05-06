let () =
  Prolog.basic_interpreter
    "
         sous_sujet(alice, groupe1).
     sous_sujet(groupe1, groupe2).
     sous_sujet(bob, groupe2).

     sous_objet(fichierA, dossier1).
     sous_objet(fichierB, dossier2).
     sous_objet(dossier1, dossier2).

     autorise(groupe1, dossier2).

     autorise(U, F) :-  sous_objet(F, D), autorise(U, D).
     autorise(U, F) :-  sous_sujet(U, G), autorise(G, F)."
    "autorise(U, F) ";
  Tests.zebra ();
  Tests.test3 ();
  Tests.test4 ();
  Tests.test5 ();
  Tests.test6 ()
