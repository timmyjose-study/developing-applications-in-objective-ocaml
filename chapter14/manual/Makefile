OCAML_OPT := ocamlopt
EXE := hello

all:
	${OCAML_OPT} -c amodule.ml
	${OCAML_OPT} -c bmodule.ml
	${OCAML_OPT} -o ${EXE} amodule.cmx bmodule.cmx

.PHONY: clean
clean:
	rm -f *.{cmx,cmi,cmo,o} ${EXE}