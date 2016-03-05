DESTDIR      = /usr/local/lib/ocaml/gles3

CC	     = gcc -fPIC
OCAMLFIND    = ocamlfind
OCAML        = ocaml
OCAMLC       = $(OCAMLFIND) ocamlc
OCAMLOPT     = $(OCAMLFIND) ocamlopt
OCAMLMKLIB   = $(OCAMLFIND) ocamlmklib
OCAMLDEP     = $(OCAMLFIND) ocamldep
INSTALL	     = $(OCAMLFIND) install
REMOVE	     = $(OCAMLFIND) remove

GLES3_CLIBS   = -cclib -lGLESv2
GLES3_CFILES  = ml_gles3.c
GLES3_MLFILES = gles3.ml matrix.ml shaders.ml buffers.ml textures.ml
GLES3_OBJS    = $(GLES3_CFILES:.c=.o) $(GLES3_MLFILES:.ml=.cmo) $(GLES3_MLFILES:.ml=.cmx)

EGL_CLIBS   = -cclib -lX11 -cclib -lEGL
EGL_CFILES  = ml_egl.c
EGL_MLFILES = egl.ml
EGL_OBJS    = $(EGL_CFILES:.c=.o) $(EGL_MLFILES:.ml=.cmo) $(EGL_MLFILES:.ml=.cmx)

DEPS = $(GLES3_MLFILES:.ml=.dep) $(EGL_MLFILES:.ml=.dep)

all: gles3.cma gles3.cmxa egl.cma egl.cmxa doc

### DEPENDENCIES
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(DEPS)
endif
endif

#main targets
gles3.cma gles3.cmxa: $(GLES3_OBJS)
	$(OCAMLMKLIB) -custom -o gles3 $(GLES3_CLIBS) $(GLES3_OBJS)

egl.cma egl.cmxa: $(EGL_OBJS)
	$(OCAMLMKLIB) -o egl $(EGL_CLIBS) $(EGL_OBJS)

# Generated C-file
gles3_tags.h: Makefile maketags.ml gles3_tags.lst
	$(OCAML) maketags.ml < gles3_tags.lst > gles3_tags.h

ml_gles3.c: gles3_tags.h

count:
	wc -l *.ml *.mli *.c | sort -n

clean:
	- rm -f *.cm[oix] *.o *~ */*~ \#* \.#* */\#* */\.#*
	- rm -f gles3_tags.h
	- rm -f *.cma *.cmxa *.so *.a *.dep
	- rm examples/*.native examples/*.byte
	- rm -rf examples/_build

distclean: clean
	- rm -rf html/*

tar: all doc clean
	cd ..; tar cvzf gles3.tar.gz gles3

install:
	mkdir -p $(DESTDIR)
	- $(REMOVE) gles3
	$(INSTALL) gles3 gles3.cma gles3.cmxa egl.cma egl.cmxa META *.so *.a *.cmi *.mli

README: README.html
	lynx $< -dump > $@

doc: README
	ocamldoc -t "OCaml GLES3 bindings" -keep-code -html -d html *.mli
	mv html/index.html html/main.html

URL=lama.univ-savoie.fr:~raffalli/WWW/gles3

tar: all clean
	cd ..; tar cvzf gles3-`date +%d%m%y`.tar.gz --exclude=_darcs gles3

distrib: all clean tar
	scp -r html/* $(URL)/
	scp -r README.html $(URL)/index.html
	scp -r cubes.png $(URL)/
	darcs push lama.univ-savoie.fr:WWW/gles3/repos
	scp gles3-`date +%d%m%y`.tar.gz $(URL)/

%.o: %.c
	$(CC) -c $<

%.cmi: %.mli %.dep
	$(OCAMLC) -c $<

%.cmo: %.ml %.cmi %.dep
	$(OCAMLC) -c $<

%.cmx: %.ml %.cmi %.dep
	$(OCAMLOPT) -c $<

%.dep: %.ml %.mli
	$(OCAMLDEP) $^ > $@
