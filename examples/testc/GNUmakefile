all: testc.native

testc.native: testc.ml $(wildcard *.glsl)
	ocamlbuild -package gles3.egl $@

.PHONY: run
run: testc.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
