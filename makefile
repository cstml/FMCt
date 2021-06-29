.PHONY: clean reformat build lint-watch compile-watch

# Run the main executable
run: build
	./result/bin/FMCt

# Build the main executable with nix
build: ./result/bin/FMCt
	nix-build release.nix

# Clean-up the folder 
clean:
	@rm .*/**~ 2> /dev/null #remove all the backups made by emacs

# Start a lint watcher 
lint-watch:
	ls **/*.hs | entr hlint .

# Start a compilation watcher 
compile-watch:
	nix-shell -p ghcid --command "ghcid"

# Reformat all the code according to fourmolu.yaml
reformat: 
	./scripts/reformat.sh
