build:
	dune build @install
	dune build bin/p1_examples.exe

install: 
	dune install  # FIXME doesn't seem to install a correct META (missing name)

uninstall:
	ocamlfind remove p1_core
# opam remove p1_core - doesn't work since dune generates an incorrect META

# dune uninstall - doesn't delete eg p1_core.cmo

clean:
	dune clean

run_examples: build
	dune exec bin/p1_examples.exe
