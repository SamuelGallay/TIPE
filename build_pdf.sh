#!/bin/bash

jupyter nbconvert --ClearOutputPreprocessor.enabled=True Vers\ la\ programmation\ logique.ipynb --to notebook --output temp.ipynb;

jupyter nbconvert  temp.ipynb --template template.tplx --to pdf --output short.pdf;

jupyter nbconvert --execute temp.ipynb --template template.tplx --to pdf --output long.pdf;

rm temp.ipynb;
