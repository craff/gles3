all: flying_cube.native

flying_cube.native: flying_cube.ml $(wildcard *.glsl)
	ocamlbuild -package gles3.egl $@

.PHONY: run
run: flying_cube.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
