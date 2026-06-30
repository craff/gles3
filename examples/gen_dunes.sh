for f in *; do
    if [ -d $f ] && [ "$f" != "." ] && [ "$f" != ".." ]; then
	echo $f
	./gen_dune.sh $f
    fi
done
