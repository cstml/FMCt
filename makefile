.PHONY: clean reformat build lint-watch compile-watch haddock-generate \
				repl-start documentation 

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
	ls **/* | entr hlint . 

lint:
	hlint .  

# Start a compilation watcher 
compile-watch:
	ghcid --command 'ghcid --command "cabal repl lib:FMCt"'

# Make the documentation and automatically refresh it 
haddock-watch:
	ls **/* | entr cabal v2-haddock executables

# Reformat all the code according to fourmolu.yaml
reformat: 
	~/.local/bin/fourmolu --mode inplace $$(git ls-tree -r --full-tree --name-only HEAD | grep -e ".*\.hs")

documentation:
	nix-shell --command "cabal v2-haddock executables"
