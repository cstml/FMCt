.PHONY: clean reformat build lint-watch compile-watch haddock-generate repl-start

# Run the main executable
run: build
	nix-shell -p rlwrap --command "rlwrap ./result/bin/FMCt"

# Build the main executable with nix
build: 
	nix-build 

# Clean-up the folder 
clean:
	@rm .*/**~ 2> /dev/null #remove all the backups made by emacs
	@rm -r ./dit-newstyle

# Starts a repl
repl-start:
	cabal new-repl Test

# Start a lint watcher 
lint-watch:
	ls **/*.hs | entr hlint .

# Start a compilation watcher 
compile-watch:
	nix-shell -p ghcid --command 'ghcid --command "cabal repl"'

# Make the documentation and automatically refresh it 
haddock-watch:
	ls **/*.hs | entr cabal new-haddock --haddock-all

# Reformat all the code according to fourmolu.yaml
reformat: 
	./scripts/reformat.sh

# Make the documentation
haddock-generate:
	haddock ./src-exe/*.hs -o ./doc -h && firefox ./doc/index.html
