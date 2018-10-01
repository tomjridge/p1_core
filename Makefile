build:
	dune build @install
	dune build bin/p1_examples.exe

install: 
	dune install

clean:
	dune clean

run_examples: build
	dune exec bin/p1_examples.exe
