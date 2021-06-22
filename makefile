.PHONY: clean reformat build

build: nix-build

clean:
	@rm .*/**~ 2> /dev/null #remove all the backups made by emacs

reformat: 
	./scripts/reformat.sh
