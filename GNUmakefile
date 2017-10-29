VERSION = 20160505.alpha

CC         = gcc -fPIC
OCAMLFIND  = ocamlfind
OCAML      = ocaml
OCAMLC     = $(OCAMLFIND) ocamlc
OCAMLOPT   = $(OCAMLFIND) ocamlopt
OCAMLMKLIB = $(OCAMLFIND) ocamlmklib
OCAMLDEP   = $(OCAMLFIND) ocamldep
INSTALL    = $(OCAMLFIND) install
REMOVE     = $(OCAMLFIND) remove
CFLAGS     = -I `ocamlfind ocamlc -where`

GLES3_CLIBS   = -cclib -lGLESv2
GLES3_CFILES  = ml_gles3.c
GLES3_MLFILES = gles3.ml vector3.ml matrix.ml shaders.ml buffers.ml textures.ml
GLES3_OBJS    = $(GLES3_CFILES:.c=.o) $(GLES3_MLFILES:.ml=.cmo) $(GLES3_MLFILES:.ml=.cmx)

EGL_CLIBS   = -cclib -lX11 -cclib -lEGL
EGL_CFILES  = ml_egl.c
EGL_MLFILES = egl.ml
EGL_OBJS    = $(EGL_CFILES:.c=.o) $(EGL_MLFILES:.ml=.cmo) $(EGL_MLFILES:.ml=.cmx)

DEPS = $(GLES3_MLFILES:.ml=.dep) $(EGL_MLFILES:.ml=.dep)

all: gles3.cma gles3.cmxa egl.cma egl.cmxa

### DEPENDENCIES
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(DEPS)
endif
endif

# Generated C-file
gles3_tags.h: maketags.ml gles3.mli gles3.ml
	$(OCAML) $^ > $@

ml_gles3.c: gles3_tags.h

#main targets
gles3.cma gles3.cmxa: $(GLES3_OBJS)
	$(OCAMLMKLIB) -custom -o gles3 $(GLES3_CLIBS) $(GLES3_OBJS)

egl.cma egl.cmxa: $(EGL_OBJS)
	$(OCAMLMKLIB) -o egl $(EGL_CLIBS) $(EGL_OBJS)

count:
	wc -l *.ml *.mli *.c | sort -n

clean:
	- rm -f *.cm[oix] *.o *~ */*~ \#* \.#* */\#* */\.#*
	- rm -f gles3_tags.h
	- rm -f *.cma *.cmxa *.so *.a *.dep
	- rm -f examples/*/*.native
	- rm -rf examples/*/_build

distclean: clean
	- rm -rf html

install:
	- $(REMOVE) gles3
	$(INSTALL) gles3 gles3.cma gles3.cmxa egl.cma egl.cmxa META *.so *.a *.cmi *.cmx *.mli

doc: all
	mkdir html
	ocamldoc -t "OCaml GLES3 bindings" -keep-code -html -d html *.mli

.PHONY: release
release: distclean
	git push origin
	git tag -a ocaml-gles3_$(VERSION)
	git push origin ocaml-gles3_$(VERSION)

%.o: %.c
	$(CC) -c $< $(CFLAGS)

%.cmi: %.mli %.dep
	$(OCAMLC) -c $<

%.cmo: %.ml %.cmi %.dep
	$(OCAMLC) -c $<

%.cmx: %.ml %.cmi %.dep
	$(OCAMLOPT) -c $<

%.dep: %.ml %.mli
	$(OCAMLDEP) $^ > $@
