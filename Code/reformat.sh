#!/bin/bash

format () {
    ocamlformat --enable-outside-detected-project --inplace --margin=100 $1
}

ListOfFiles=("parser.ml"
             "solver.ml"
             "types.ml"
             "unification.ml"
             "utilitary.ml")
 
echo "Reformating files :"
for fileName in ${ListOfFiles[*]}; do
    echo $fileName
    format $fileName
done
