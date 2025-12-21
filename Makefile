.PHONY: build test clean help

## build - Build the library
build:
	stack build

## test - Run the test suite
test:
	stack test

## clean - Remove build artifacts
clean:
	stack clean

## help - Show this help message
help:
	@grep -E '^## ' Makefile | sed 's/## //'
