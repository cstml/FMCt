.PHONY: clean reformat build

run: build
	./result/bin/FMCt

build: ./result/bin/FMCt
	nix-build release.nix

clean:
	@rm .*/**~ 2> /dev/null #remove all the backups made by emacs

reformat: 
	./scripts/reformat.sh
