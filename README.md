# MeQuieroVolver
Un bot de telegram cuya función es brindar información acerca de los próximos horarios de colectivos de la terminal de ómnibus de rosario.

## Como funciona?
Para empezar debe iniciar una charla con el bot desde la aplicacion Telegram, buscandolo con el nombre MeQuieroVolver.

## Comandos
Las consultas que puede realizar son:
Ciudades: Muestra una lista de ciudades disponibles.
Viajes a/desde CIUDAD: Muestra los próximos 3 viajes.
Viajes a/desde CIUDAD, ver n: Muestra los próximos n viajes.
Viajes a/desde CIUDAD, ver todos: Muestra todos los viajes.
Viajes a/desde CIUDAD, entre HORA y HORA: Muestra viajes entre el rango horario dado.
Guardar NOMBRE = COMANDO: Almacena temporalmente un comando con el nombre seleccionado para ser ejecutado con el comando mostrar (las variables deben ser cadenas de caracteres en minúscula y/o numeros).
Mostrar NOMBRE: Ejecuta el comando almacenado en NOMBRE.
Queja MENSAJE: En caso de encontrar algun problema con el bot pueden reailizar una queja.

## Ejemplos:
"Viajes a Carcarañá, entre 12:00 y 15:30".
"Viajes desde 9 de Julio (BA), ver 5".
"Guardar achabas = Viajes a Chabas".
"Mostrar achabas".
Respuesta:
Los Ranqueles, sale 12:00 y llega 13:00
Días disponibles: :white_check_mark: :negative_squared_cross_mark: :white_check_mark: :white_check_mark: :white_check_mark: :white_check_mark: :white_check_mark: :negative_squared_cross_mark: (indicando que no esta disponibles los días Martes y feriados). 
