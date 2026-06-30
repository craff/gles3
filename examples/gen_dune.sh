#!/bin/bash

set -e

NAME="$1"
DEST="dune"

cd $1

if [ -z "$NAME" ]; then
    echo "usage: $0 example_name"
    exit 1
fi

MODULE=${NAME^}

cat > ${DEST} <<EOF
(rule
 (target ${NAME}_x11.ml)
 (action (write-file %{target} "include ${MODULE}")))

(rule
 (target ${NAME}_wayland.ml)
 (action (write-file %{target} "include ${MODULE}")))

EOF

for shader in shaders/*.glsl
do
    [ -e "$shader" ] || continue

    mod=$(basename "$shader" .glsl)

    cat >> ${DEST} <<EOF
(rule
 (target ${mod}.ml)
 (deps ${shader})
 (action
  (with-stdout-to %{target}
   (run glsl_to_ml %{deps}))))

EOF

done

cat >> ${DEST} <<EOF
(executable
 (name ${NAME}_x11)
 (public_name ${NAME}_x11)
 (modules ${NAME}_x11 ${NAME} $(for s in shaders/*.glsl; do [ -f "$s" ] && echo -n $(basename ${s%.*}) " "; done))
 (flags (:standard -w -6))
 (libraries gles3 gles3-x11)
 (package gles3))

(executable
 (name ${NAME}_wayland)
 (public_name ${NAME}_wayland)
 (modules ${NAME}_wayland ${NAME} $(for s in shaders/*.glsl; do [ -f "$s" ] && echo -n $(basename ${s%.*}) " "; done))
 (flags (:standard -w -6))
 (libraries gles3 gles3-wayland)
 (package gles3))
EOF
