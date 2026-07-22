VERSION = 20203112.alpha

HAVE_X11 := $(shell pkg-config --exists x11 egl && echo true || echo false)
HAVE_WAYLAND := $(shell pkg-config --exists wayland-client wayland-egl egl && echo true || echo false)

export BUILD_X11_EXAMPLES := $(HAVE_X11)
export BUILD_WAYLAND_EXAMPLES := $(HAVE_WAYLAND)

all:
	@dune build @install
.PHONY: all

count:
	@wc -l lib/*.mlc lib/*.ml lib/*.mli lib/*.c | sort -n
.PHONY: count

clean:
	@dune clean
.PHONY: clean

install:
	@dune install
.PHONY: install

uninstall:
	@dune uninstall
.PHONY: uninstall

doc:
	@dune build @doc

.PHONY: release
release: distclean
	git push origin
	git tag -a ocaml-gles3_$(VERSION)
	git push origin ocaml-gles3_$(VERSION)

install_doc: doc
	rsync -r --delete _build/default/_doc/_html/ ~/WWW2/main/gles3/manual/
