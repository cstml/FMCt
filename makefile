.PHONY: clean reformat build lint-watch compile-watch haddock-generate \
				repl-start documentation 

# Run the main executable
run: build
	cabal new run FMCt-web

# Build the main executable with nix
build: 
	cabal new-build

# Starts a repl
repl-start:
	cabal new-repl 

# Start a lint watcher 
lint-watch:
	ls **/* | entr hlint . 

lint:
	hlint .  

# Start a compilation watcher 
compile-watch:
	ghcid --command 'ghcid --command "cabal repl lib:FMCt"'

# Make the documentation and automatically refresh it 
haddock-watch:
	ls **/* | entr cabal v2-haddock executables

# Will create the documentation and then open a browser with it 
documentation:
	cabal v2-haddock executables\
  && open ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/FMCt-0.5.0.0/x/FMCt-web/doc/html/FMCt/FMCt-web/index.html \
	|| firefox ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/FMCt-0.5.0.0/x/FMCt-web/doc/html/FMCt/FMCt-web/index.html \
  || echo "it seems like I cannot find the index. Look at the last line as it should indicate where the index.html is saved. Open it in a browser."
