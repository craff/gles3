all: curves.native

curves.native: curves.ml $(wildcard *.glsl)
	ocamlbuild -package gles3.egl $@

.PHONY: run
run: curves.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
