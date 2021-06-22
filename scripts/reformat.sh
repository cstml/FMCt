for f in ./src-exe/*.hs
do
    echo "Reformatting $f"
    fourmolu -i $f # -i is inplace
done
    
