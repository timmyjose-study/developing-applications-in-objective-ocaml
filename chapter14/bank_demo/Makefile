all:
	ocamlc -c log.ml
	ocamlc -c account.ml
	ocamlfind ocamlc -package unix -linkpkg -o demo log.cmo account.cmo main.ml

.PHONY: clean
clean:
	rm -f *.{cmi,cmx,cmo,o} demo