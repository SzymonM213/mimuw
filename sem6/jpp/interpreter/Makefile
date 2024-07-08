.PHONY: build clean

build:
	cabal build
	cp dist-newstyle/build/x86_64-linux/ghc-*/corretto-interpreter-0.1.0.0/x/corretto-interpreter/build/corretto-interpreter/corretto-interpreter interpreter

clean:
	rm -f interpreter
	cabal clean