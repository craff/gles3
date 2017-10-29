all: testd.native

testd.native: testd.ml $(wildcard *.glsl)
	ocamlbuild -package gles3.egl $@

.PHONY: run
run: testd.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
