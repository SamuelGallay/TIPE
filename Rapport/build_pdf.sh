#!/bin/bash

nbmerge Rapport.ipynb Code.ipynb > merged.ipynb

jupyter nbconvert --ClearOutputPreprocessor.enabled=True merged.ipynb --to notebook --output temp.ipynb;

jupyter nbconvert  temp.ipynb --template template.tplx --to pdf --output document.pdf;

rm merged.ipynb temp.ipynb;
