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
  the following framework are supported:
  - X11+EGL
  - Wayland+EGL

  We would like to support different platforms in the future
  (Windows, Android, iOS, ...) without changing the interface.

## Authors

* [Alexandre Miquel](https://www.fing.edu.uy/~amiquel) (initial low level
  bindings for GLES 2)
* [Christophe Raffalli](https://lama.univ-savoie.fr/~raffalli) (partial port
  to GLES 3.0, high-level bindings and examples)
* [Rodolphe Lepigre](https://lepigre.fr) (maze example and port to the dune
  build system)

## Installation

To install from source, clone the repository and run `make && make install`.
Alternatively, you can pin the repository using the following commands (only
one of latest two is useful):
```bash
opam pin add gles3-stubs.dev https://github.com/craff/gles3.git
opam pin add gles3.dev https://github.com/craff/gles3.git
opam pin add gles3-x11.dev https://github.com/craff/gles3.git
opam pin add gles3-wayland.dev https://github.com/craff/gles3.git
```

## Documentation

The OCaml documentation can be extracted to a webpage using `make doc`. The
generate page is the placed at `_build/default/_doc/_html/index.html`).

The repository also contains multiple examples:
* `examples/flying_cube`: a rotating cube.
* `examples/curves`: a wire-frame cube
* `examples/exploding_cube`: the names says it all
* `examples/cubeTexture`: a rotating cube with a texture (including a text).
* `examples/cubesAndShapes`: rotating cubes and 8 implicit surfaces with
  simple shadow mapping.
* `examples/spheres`: many bouncing spheres, using `domain` to run multicore.
* `examples/maze`: a maze generator with a simple exploration camera.
* `examples/click`: illustrates how to use a shader to click on object and
   get the ID of the object and the position of the clicked point in 3D.
* `examples/window`: flying_cube in two distinct windows.

**To run the examples:** to run an example, move to its directory and run one
of the two following commands
```BUILD_X11_EXAMPLES=true dune exec -- <name>_x11.exe
   BUILD_WAYLAND_EXAMPLES=true dune exec -- <name>_wayland.exe
```
where name is the name of the file defining the example without extension.
example: `flying_cube_x11.exe` for the first example.

## Contributing

Contributions are very welcome!

Things to do:
* Test a lot.
* Write more documentation.
* Complete the low-level bindings (see `PRIMITIVES`).
* Improve the high-level bindings guided by more examples.
* Provide support for more platforms.
* Find a way to support multiple version of GLES (at least 2.0, 3.0, 3.1, 3,2)
  with only one OCaml library, and possibly in a transparent way when using
  the high-level bindings (i.e., the high level bindings should test the
  version and make the best choice).
* Collect regexp for error message in GLSL for most GLES vendors to give good
  error messages in all cases (only nouveau/mesa is currently supported). It
  seems that OpenGL ES does not standardize even error message positions.
