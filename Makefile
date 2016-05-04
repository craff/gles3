MAJOR = 20160307
MINOR = alpha
VERSION = $(MAJOR).$(MINOR)

CC	     = gcc -fPIC
OCAMLFIND    = ocamlfind
OCAML        = ocaml
OCAMLC       = $(OCAMLFIND) ocamlc
OCAMLOPT     = $(OCAMLFIND) ocamlopt
OCAMLMKLIB   = $(OCAMLFIND) ocamlmklib
OCAMLDEP     = $(OCAMLFIND) ocamldep
INSTALL	     = $(OCAMLFIND) install
REMOVE	     = $(OCAMLFIND) remove
CFLAGS	     = -I `ocamlfind ocamlc -where`

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
	- rm -rf html/* opam

install:
	- $(REMOVE) gles3
	$(INSTALL) gles3 gles3.cma gles3.cmxa egl.cma egl.cmxa META *.so *.a *.cmi *.mli

README: README.html
	lynx $< -dump > $@

doc: README
	ocamldoc -t "OCaml GLES3 bindings" -keep-code -html -d html *.mli
	mv html/index.html html/main.html

URLSSH=lama.univ-savoie.fr:WWW/gles3
URL=https://lama.univ-savoie.fr/~raffalli/gles3

tar:
	cd ../gles3_0; darcs pull; make all doc clean
	cd ..; tar cvfz gles3-$(VERSION).tar.gz --exclude=_darcs --transform "s,gles3_0,gles3-$(VERSION),"  gles3_0

distrib: all doc clean tar
	scp -r html/* $(URLSSH)/
	scp -r README.html $(URLSSH)/index.html
	scp -r cubes.png $(URLSSH)/
	darcs push lama.univ-savoie.fr:WWW/gles3/repos
	scp ../gles3-$(VERSION).tar.gz $(URLSSH)/
	ssh lama.univ-savoie.fr "cd WWW/gles3; ln -sf gles3-$(VERSION).tar.gz gles3-latest.tar.gz"

OPAMREPO=$(HOME)/Caml/opam-repository/packages/gles3

opam: opam.tmpl distrib
	sed -e s/__VERSION__/$(VERSION)/g opam.tmpl > opam
	mkdir -p $(OPAMREPO)/gles3.$(VERSION)
	cp opam $(OPAMREPO)/gles3.$(VERSION)/opam
	cp description.txt $(OPAMREPO)/gles3.$(VERSION)/descr
	echo -n "archive: \""  > $(OPAMREPO)/gles3.$(VERSION)/url
	echo -n "$(URL)/gles3-$(VERSION).tar.gz" >> $(OPAMREPO)/gles3.$(VERSION)/url
	echo "\"" >> $(OPAMREPO)/gles3.$(VERSION)/url
	echo -n "checksum: \"" >> $(OPAMREPO)/gles3.$(VERSION)/url
	echo -n `md5sum ../gles3-$(VERSION).tar.gz | cut -b -32` >> $(OPAMREPO)/gles3.$(VERSION)/url
	echo "\"" >> $(OPAMREPO)/gles3.$(VERSION)/url

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
