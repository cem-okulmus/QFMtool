#!/bin/sh
ocamlfind ocamlc -custom -g -thread -package qfm -ccopt -no-pie -linkpkg lablrsvg.cma  qfmtool.ml -o  QFMtool
rm qfmtool.c*
