for f in ./src-exe/*.hs
do
    echo "Reformatting $f"
    fourmolu $f > $f
done
    
