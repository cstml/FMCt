for f in ./src/**/*.hs
do
    echo "Reformatting $f"
    fourmolu -m inplace $f 
done
    
