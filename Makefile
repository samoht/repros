.PHONY: readdir

readdir:
	ocamlfind ocamlopt -package mtime.os,lwt.unix readdir.ml -o readdir -linkpkg
