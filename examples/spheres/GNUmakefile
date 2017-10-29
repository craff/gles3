all: spheres.native

spheres.native: spheres.ml $(wildcard *.glsl)
	ocamlbuild -pkgs ancient,gles3.egl $@

.PHONY: run
run: spheres.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
