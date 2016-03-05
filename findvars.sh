#!/bin/bash

cat gles2.mli gles2.ml | grep '`' |
sed 's/[^`]*`\([A-Za-z][A-Za-z0-9_]*\)[^`]*/\1\n/g' |
sort | uniq | grep '.'
