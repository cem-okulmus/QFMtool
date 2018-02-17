#!/bin/sh
ocamlfind ocamlc -custom  -package qfm -thread -linkpkg -ccopt "-subsystem windows"  qfmtool.ml -o  QFMtool
rm qfmtool.c*
