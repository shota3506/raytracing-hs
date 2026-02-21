.PHONY: build run format

build:
	@cabal build

run:
	@cabal run

format:
	@find . -name "*.hs" -exec ormolu -i {} +
