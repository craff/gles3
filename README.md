# [OCaml](http://caml.inria.fr/) [GLES](https://www.khronos.org/registry/gles) 3.0 bindings

![Screen shot](https://raw.githubusercontent.com/craff/gles3/master/cubes.png?raw=true "A nice screen shot")


## Introduction

This project aims at providing a portable way to do OpenGL (precisely
GLES) application using OCaml. It comes in three parts:
* Low level bindings which allow to call directly GLES functions.
  This binding tries to be reasonably type-safe using polymorphic
  variants to encode Glenum type. The low level bindings also provide
  some sanity checks for the size of bigarrays which allow to capture
  quite a lot of errors with clear messages.
* High level bindings: to provide some auxiliary functions like
  matrix inversion and ease the development. For instance, to use
  shaders, with the high level bindings, you use compile_shader with
  the sources code, get a value of type unit program. Then, you can
  set the variables of the shaders (uniform or attributes), either as
  constant or function and get a function to finally run the shaders.
* A way to open a window, start the main loop and interact. Currently
  only EGL under X11 is supported but it would be nice to have
  support for other platforms (windows, OSX, android, ios, wayland,
  ...) with exactly the same interface.

## Authors

* [Alexandre Miquel](https://www.fing.edu.uy/~amiquel) (initial low level bindings for GLES 2)
* [Christophe Raffalli](https://lama.univ-savoie.fr/~raffalli) (partial port to GLES 3.0, high-level
       bindings and examples)

## Installation

* as an opam package
* [from the gles3 github repository](https://github.com/craff/gles3)

## Documentation

* Generated html from the MLI files (make doc)
* the examples:
  * `./examples/testa_gles.ml`: a rotating cube
  * `./examples/testb_gles.ml`: a rotating cube with a texture
  * `./examples/testc_gles.ml`: 7 rotating cubes with simple shadow mapping
  * `./examples/testd_gles.ml`: 7 rotating cubes and 8 implicit
                                 surfaces with simple shadow mapping
  * `./examples/spheres.ml`   : mainy boucing spheres, using ancient
                                 to run multicore (experimental)

## TODO (please contribute !)

* test and fix bugs !
* documentation
* complete the low-level bindings
* develop the high-level bindings guided by more examples
* provide support for more platforms (see introduction)
* find a way to support multiple version of GLES (at least 2.0, 3.0,
  3.1) with only one OCaml library, and possibly in a transparent way
  when using the high-level bindings (i.e., the high level bindings
  should test the version and make the best choice)
* collect regexp for error message in GLSL for most GLES vendors to
  give good error messages in all cases (only one vendor,
  nouveau/mesa supported currently). This is a pity that GLES does no
  define a standard at least for position in error messages.
* ...
