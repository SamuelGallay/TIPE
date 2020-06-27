#!/bin/bash

format () {
    ocamlformat --enable-outside-detected-project --inplace --break-string-literals=never --margin=100 $1
}

ListOfFiles=("parser.ml"
             "solver.ml"
             "types.ml"
             "unification.ml"
             "utilitary.ml"
             "tests.ml"
             "main.ml"
             "normalize.ml")

 
echo "Reformating files :"
for fileName in ${ListOfFiles[*]}; do
    echo $fileName
    format $fileName
done
