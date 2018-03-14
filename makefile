install:
	@echo "Instalando..."
	@stack setup
	@stack build

run:
	@echo "Ejecutando Programa..."
	@stack exec MeQuieroVolver-exe
