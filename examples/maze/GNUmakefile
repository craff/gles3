all: maze.native

maze.native: $(wildcard *.ml) $(wildcard *.glsl)
	ocamlbuild -package gles3.egl $@

.PHONY: run
run: maze.native
	./$^

clean:
	rm -f *~
	ocamlbuild -clean
