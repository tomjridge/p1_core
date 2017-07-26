all:
	ocamlfind ocamlopt -package tjr_lib -c p1_core.ml p1_examples.ml
	ocamlfind ocamlopt -package tjr_lib -linkpkg -o p1_examples.native p1_core.cmx p1_examples.cmx

clean:
	rm -f *.{cmi,cmo,cmx,o}
