all:
	ocamlfind ocamlopt -package tjr_lib -c p1_core.ml


clean:
	rm -f *.{cmi,cmo,cmx,o}
