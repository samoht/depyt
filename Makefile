.PHONY: all clean doc

all:
	ocaml pkg/pkg.ml build --tests true
	ocaml pkg/pkg.ml test

clean:
	ocaml pkg/pkg.ml clean

doc:
	topkg doc -r
