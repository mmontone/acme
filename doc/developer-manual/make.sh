#!/bin/sh

make html
make latexpdf

cp _build/latex/configurator.pdf .

rm -rf html
cp -r _build/html .
