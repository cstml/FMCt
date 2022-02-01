.PHONY: clean reformat build lint-watch compile-watch haddock-generate \
				repl-start documentation test all-files tags

tags:
	hasktags . 

# Run the main executable
run: build
	cabal new run FMCt-web

# Build the main executable with nix
build:
	cabal new-build

# Starts a repl
repl:
	cabal new-repl

test:
	cabal new-test all

lint:
	hlint .

# Start a lint watcher
watch-lint:
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs' | entr make lint 

# Start a compilation watcher
watch-compile:
	ghcid --command 'ghcid --command "cabal repl lib:FMCt"'

# Start a compilation watcher
watch-all:
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs' | entr cabal new-build all

watch-test:
	make all-files | entr make test

all-files:
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs'

# Make the documentation and automatically refresh it
haddock-watch:
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs' | entr cabal v2-haddock executables

# Will create the documentation and then open a browser with it
documentation:
	cabal v2-haddock executables\
    && open ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/FMCt-0.5.0.0/x/FMCt-web/doc/html/FMCt/FMCt-web/index.html \
	|| firefox ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/FMCt-0.5.0.0/x/FMCt-web/doc/html/FMCt/FMCt-web/index.html \
    || echo "it seems like I cannot find the index. Look at the last line as it should indicate where the index.html is saved. Open it in a browser."

# Add folder locations to the list to be reformatted.
fourmolu-format:
	fourmolu \
		--mode inplace \
		--check-idempotence\
		-o -XTypeApplications \
		-o -XTemplateHaskell \
		-o -XPatternSynonyms \
		-o -fplugin=RecordDotPreprocessor\
	    $$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')
