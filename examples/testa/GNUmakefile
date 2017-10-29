all: testa.native

testa.native: testa.ml $(wildcard *.glsl)
	ocamlbuild -package gles3.egl $@

.PHONY: run
run: testa.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
