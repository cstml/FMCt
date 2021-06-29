.PHONY: clean reformat build lint-watch compile-watch haddock-generate repl-start

# Run the main executable
run: build
	./result/bin/FMCt

# Build the main executable with nix
build: ./result/bin/FMCt
	nix-build release.nix

# Clean-up the folder 
clean:
	@rm .*/**~ 2> /dev/null #remove all the backups made by emacs

# Starts a repl
repl-start:
	cabal new-repl

# Start a lint watcher 
lint-watch:
	ls **/*.hs | entr hlint .

# Start a compilation watcher 
compile-watch:
	nix-shell -p ghcid --command "ghcid"

# Make the documentation and automatically refresh it 
haddock-watch:
	ls **/*.hs | entr haddock ./src-exe/*.hs -o ./doc -h

# Reformat all the code according to fourmolu.yaml
reformat: 
	./scripts/reformat.sh

# Make the documentation
haddock-generate:
	haddock ./src-exe/*.hs -o ./doc -h && firefox ./doc/index.html
