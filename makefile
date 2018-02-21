install:
	@echo "Instalando..."
	@cabal update	
	@cabal install MeQuieroVolver.cabal
	@cd src;make all
	mv src/MeQuieroVolver MeQuieroVolver
