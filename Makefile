.PHONY: build test clean bench help

## build - Build the library
build:
	stack build

## test - Run the test suite
test:
	stack test

## bench - Run runtime benchmarks (using benchpress library)
bench:
	stack bench

## clean - Remove build artifacts
clean:
	stack clean

## help - Show this help message
help:
	@grep -E '^## ' Makefile | sed 's/## //'
