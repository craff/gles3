#!/bin/sh

for f in *; do
    if [ -d "$f" ] ; then
	echo ${f}_x11
	dune exec ${f}/${f}_x11.exe
	echo ${f}_wayland
	dune exec ${f}/${f}_wayland.exe
    fi
done
