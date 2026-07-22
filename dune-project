(lang dune 3.00)
(name gles3)
(generate_opam_files true)

(dialect
  (name glsl)
  (implementation
     (extension glsl))
  (interface
     (extension glsli)))

(authors Christophe Raffalli)
(maintainers Christophe Raffalli)
(source (github craff/gles3))
(homepage https://raffalli.eu/gles3/)
(documentation https://raffalli.eu/gles3)
(bug_reports https://github.com/craff/gles3/issues)
(license MIT)

(package
	(name gles3)
	(synopsis "OpenGL ES 3.2 bindings")
	(description
	  "This project aims at providing a portable way to write OpenGL (more precisely\
 	   OpenGL ES) application in OCaml. It comes in three parts:\
           * Low level bindings allowing direct calls to OpenGL ES functions. These\
             bindings try to remain reasonably type-safe using polymorphic variants\
             to encode enumeration types. The low level bindings also provide some\
             sanity checks for the size of bigarrays which allow to capture quite a\
             lot of errors with clear messages.\
           * High level bindings providing auxiliary functions like matrix inversion\
             to ease the development. For instance, to use shaders with the high\
             level bindings, you may use `compile_shader` with the sources code, to\
             get a value of type `unit program`. Then, you can set the variables of\
             the shaders (uniform or attributes), either as constant or function and\
             get an OCaml function to finally run the shaders.\
           * A wrapper to open a window, start the main loop and interact. Currently\
             only EGL under X11 is supported, but we would like to support different\
             platforms in the future (Windows, Android, iOS, Wayland, ...) without\
            changing the interface.")
	(tags (graphics 3D openGL Gles))
	(depends
	   (ocaml (>= 5.0.0))))

(package
 (name gles3-stubs)
 (synopsis "OpenGL ES3, Commen Egl and Gles3 C stubs")
 (allow_empty))

(package
 (name gles3-x11)
 (synopsis "OpenGL ES3 X11 backend")
 (allow_empty)
 (depends (gles3 (= :version)) conf-libx11))

(package
 (name gles3-wayland)
 (synopsis "OpenGL ES3 Wayland backend")
 (allow_empty)
 (depends (gles3 (= :version)) conf-libwayland))
