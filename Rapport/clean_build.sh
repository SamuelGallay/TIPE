#!/bin/bash

rm -rf rapport.aux rapport.bbl rapport.blg rapport.log rapport.out rapport.synctex.gz _minted-rapport

pdflatex -shell-escape rapport.tex

bibtex rapport

pdflatex -shell-escape rapport.tex

pdflatex -shell-escape rapport.tex
