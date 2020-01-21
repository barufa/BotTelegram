VERSION := $(shell stack --version | grep Version | sed 's/Version//g' | sed 's/^ *//g' | cut -d "," -f 1)

install:
	@echo "Instalando..."
	@if [ $(VERSION) != "1.5.1" ]; then\
		@stack upgrade --binary-version 1.5.1;\
	fi	
	@stack setup
	@stack build

stack:
	wget -qO- https://get.haskellstack.org/ | sh

run:
	@echo "Iniciando Programa..."
	@stack exec MeQuieroVolver-exe

