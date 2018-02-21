install:
	@echo "Instalando..."
	@cabal install MeQuieroVolver.cabal
	@cd src;make all
	mv src/MeQuieroVolver MeQuieroVolver
