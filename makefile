install:
	@echo "Instalando..."
	@stack setup
	@stack build

run:
	@echo "Iniciando Programa..."
	@stack exec MeQuieroVolver-exe
