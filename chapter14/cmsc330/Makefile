all:
	ocamlc -c stack.mli
	ocamlc -c list_stack.ml
	ocamlc -o demo list_stack.cmo main.ml

.PHONY: clean
clean:
	rm -f *.{cmo,cmx,cmi,o} demo

