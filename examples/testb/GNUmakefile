all: testb.native

testb.native: testb.ml $(wildcard *.glsl)
	ocamlbuild -package gles3.egl $@

.PHONY: run
run: testb.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
