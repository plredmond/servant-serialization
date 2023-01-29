# this file assumes you're already in a nix-shell
#
# this file builds using setup commands (not nix-build)

CABAL_FILE = servant-serialization.cabal

CONFIG_FILE = dist/setup-config
SETUP_CMD = runhaskell Setup.hs

.PHONY: test build clean repl

test: build
	$(SETUP_CMD) test

# TODO use dist/build/%/% ? scan cabalfile for executable names?
build: $(CONFIG_FILE)
	$(SETUP_CMD) build

$(CONFIG_FILE): $(CABAL_FILE)
	$(SETUP_CMD) configure --enable-tests

clean: $(CABAL_FILE)
	$(SETUP_CMD) clean

%.cabal: package.yaml
	hpack

## tools

repl: $(CONFIG_FILE)
	$(SETUP_CMD) repl $(basename $(CABAL_FILE))

ghcid:
	nix-shell -p ghcid --run 'ghcid -c make repl'
entr-build:
	git ls-files | entr -c bash -c 'time make build'
entr-test:
	git ls-files | entr -c bash -c 'time make test'
