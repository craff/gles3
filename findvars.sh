#!/bin/bash

cat gles3.mli gles3.ml | grep '`' |
sed 's/[^`]*`\([A-Za-z][A-Za-z0-9_]*\)[^`]*/\1\n/g' |
sort | uniq | grep '.'
