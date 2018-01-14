all: build remove

build:
	ghc Main.hs
	mv Main MeQuieroVolver

remove:
	rm -f *.hi *.o

clean: remove
	rm -f Main MeQuieroVolver


