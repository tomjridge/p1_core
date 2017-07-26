SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV

all: FORCE
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	$$mk_cma -g -a -o $$libname.cma $$cmos
	$$mk_cmxa -g -a -o $$libname.cmxa $$cmxs
	$(MAKE) install

# note this builds against the installed package
p1_examples.native: FORCE
	$$ocamlopt -package $$libname -linkpkg -o p1_examples.native p1_examples.ml


install:
	-rm -f p1_examples.{o,cmi,cmx} # don't want any of these 
	-ocamlfind remove $$package_name
	mk_meta
	ocamlfind install $$package_name META *.cmi *.o *.a *.cma *.cmxa *.cmo *.cmx 

clean:
	rm -f *.{cmi,cmo,cmx,o}

FORCE:
