.PHONY: all clean doc test

all:
	dune build

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean
