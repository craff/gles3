#!/bin/sh

#needs make install in the parent directory and
#use both ocamlfind and ocamlbuild.

ocamlbuild -pkgs gles3.egl -use-ocamlfind testa_gles.byte
ocamlbuild -pkgs gles3.egl -use-ocamlfind testb_gles.byte
ocamlbuild -pkgs gles3.egl -use-ocamlfind testc_gles.byte
ocamlbuild -pkgs gles3.egl -use-ocamlfind testd_gles.byte

ocamlbuild -pkgs gles3.egl -use-ocamlfind testa_gles.native
ocamlbuild -pkgs gles3.egl -use-ocamlfind testb_gles.native
ocamlbuild -pkgs gles3.egl -use-ocamlfind testc_gles.native
ocamlbuild -pkgs gles3.egl -use-ocamlfind testd_gles.native

#if ancient is installed
#ocamlbuild -pkgs ancient,gles3.egl -use-ocamlfind spheres.byte
#ocamlbuild -pkgs ancient,gles3.egl -use-ocamlfind spheres.native
