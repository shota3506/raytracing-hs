.PHONY: setup build run lint test format

setup:
	@cabal install ormolu --overwrite-policy=always
	@cabal install hlint --overwrite-policy=always

build:
	@cabal build

run:
	@cabal run

lint:
	@hlint src/ app/ test/

test:
	@cabal test

format:
	@find . -name "*.hs" -exec ormolu -i {} +
