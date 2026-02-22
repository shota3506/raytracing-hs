.PHONY: build run test format

build:
	@cabal build

run:
	@cabal run

test:
	@cabal test

format:
	@find . -name "*.hs" -exec ormolu -i {} +
