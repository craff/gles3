# [OCaml](http://caml.inria.fr/) [OpenGL ES](https://www.khronos.org/registry/gles) 3.0 bindings

![Screen shot](https://raw.githubusercontent.com/craff/gles3/master/cubes.png?raw=true "A nice screen shot")

## Introduction

This project aims at providing a portable way to write OpenGL (more precisely
OpenGL ES) application in OCaml. It comes in three parts:
* Low level bindings allowing direct calls to OpenGL ES functions. These
  bindings try to remain reasonably type-safe using polymorphic variants
  to encode enumeration types. The low level bindings also provide some
  sanity checks for the size of bigarrays which allow to capture quite a
  lot of errors with clear messages.
* High level bindings providing auxiliary functions like matrix inversion
  to ease the development. For instance, to use shaders with the high
  level bindings, you may use `compile_shader` with the sources code, to
  get a value of type `unit program`. Then, you can set the variables of
  the shaders (uniform or attributes), either as constant or function and
  get an OCaml function to finally run the shaders.
* A wrapper to open a window, start the main loop and interact. Currently
  only EGL under X11 is supported, but we would like to support different
  platforms in the future (Windows, Android, iOS, Wayland, ...) without
  changing the interface.

## Authors

* [Alexandre Miquel](https://www.fing.edu.uy/~amiquel) (initial low level bindings for GLES 2)
* [Christophe Raffalli](https://lama.univ-savoie.fr/~raffalli) (partial port to GLES 3.0, high-level bindings and examples)
* [Rodolphe Lepigre](https://lepigre.fr) (maze example and port to the dune build system)

## Installation

To install from source, clone the repository and run `make && make install`.
Alternatively, you can pin the repository using the following command:
```bash
opam pin add gles3.dev https://github.com/craff/gles3.git
```

## Documentation

The OCaml documentation can be extracted to a webpage using `make doc`. The
generate page is the placed at `_build/default/_doc/_html/index.html`).

The repository also contains multiple examples:
* `examples/testa/testa.ml`: a rotating cube.
* `examples/testb/testb.ml`: a rotating cube with a texture.
* `examples/testc/testc.ml`: 7 rotating cubes with simple shadow mapping.
* `examples/testd/testd.ml`: 7 rotating cubes and 8 implicit surfaces with simple shadow mapping.
* `examples/spheres/spheres.ml`: mainy boucing spheres, using `ancient` to run multicore (experimental).
* `examples/maze/maze.ml`: a maze generator with a simple exploration camera.
* `examples/fluid_cube/fluid_cube.ml`
* `examples/curves/curves.ml`

**To run the examples:** to run an example, move to its directory and run the
command `dune exec -- <name>`, where name is `testa` for the first example,
and similarly for the others.

## Contributing

Contributions are very welcome!

Things to do:
* Test a lot.
* Write more documentation.
* Complete the low-level bindings (see `PRIMITIVES`).
* Imprive the high-level bindings guided by more examples.
* Provide support for more platforms.
* Find a way to support multiple version of GLES (at least 2.0, 3.0, 3.1, 3,2)
  with only one OCaml library, and possibly in a transparent way when using
  the high-level bindings (i.e., the high level bindings should test the
  version and make the best choice).
* Collect regexp for error message in GLSL for most GLES vendors to give good
  error messages in all cases (only nouveau/mesa is currently supported). It
  seems that OpenGL ES does not standardize even error message positions.
