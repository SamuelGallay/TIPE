#!/bin/bash

rm -f prolog.cmi prolog.cmo prolog.ml

jupyter nbconvert  Vers\ la\ programmation\ logique.ipynb --to script --output prolog

ocamlc -c prolog.mli

ocamlc -c prolog.ml

rm -f prolog.ml
