{-# LANGUAGE OverloadedStrings #-}

module Data (
 module Data.Text,
 module Data.Char,
 module Types,
 init_mem,
 lookfor,
 update,
 printCiudades,
 isCiudad,
 codigo,
 perhaps,
 trcd,
 mensaje_help
 ) where

import BST
import Map
import Types
import Web.Telegram.API.Bot
import Data.Char              (toLower)
import Data.Text              (pack,unpack,Text)
import Data.Maybe             (isNothing)
import Data.Set.BKTree        (fromList,closest)

---------------------------
---Funciones Principales---
---------------------------

init_mem :: Map a b
init_mem = emptyMap

lookfor :: ChatId->Variable->Memoria->Error Comand--Dado un chat, una variable y un entorno me busca la variable v en el entorno
lookfor (ChatId n) v m = case lookfor' (fromIntegral n :: Integer) v m of
                           Nothing -> Err ("La variable "++v++" no esta definida")
                           Just c  -> Result c
                         where lookfor' id v m = search m id >>=(\t->search t v)

update :: ChatId -> Variable -> Comand -> Memoria -> Memoria --Dado una chat, una variable y un comando, me lo almacena en la memoria correspondiente
update (ChatId n) v c m = let id = (fromIntegral n :: Integer)
                           in case search m id of--Busco el arbol correspondiente
                                   Nothing -> insert m id (insert emptyMap v c)--Si no existe, lo creo 
                                   Just t  -> insert m id (insert t v c)--Si existe, lo actualizo

printCiudades :: Int -> Int -> String
printCiudades a b = printCiudades' (take (b-a) (drop a list_ciudades))
                    where printCiudades' [] = "\n" :: String
                          printCiudades' (x:xs) = (fst x)++"\n"++(printCiudades' xs)


codigo :: Ciudad -> Maybe Int
codigo c = do{(s,n) <- find c;return n}

isCiudad :: Ciudad -> Bool
isCiudad c = not $ isNothing $ find c

trcd :: Ciudad -> Ciudad--Pasa una ciudad de minuscula a mayuscula
trcd c = case find c of
           Just x  -> fst x
           Nothing -> c

perhaps :: Ciudad->String
perhaps c = case closest c bktree_ciudades of
              Just x  ->"Quizás quiso decir: "++trcd (fst x)++"\n"
              Nothing ->"\n"

--------------------------
---Funciones auxiliares---
--------------------------

find :: Ciudad -> Maybe (Ciudad,Int)
find c = searchBy src tree_ciudades c
         where src x (y,z) = compare (ml x) (ml y)
               ml = map toLower

mensaje_help="Las consulta que puede realizar son:\nCiudades: Muestra una lista de ciudades disponibles\nViajes a/desde CIUDAD: Muestra los próximos 3 viajes\nViajes a/desde CIUDAD, ver n: Muestra los próximos n viajes\nViajes a/desde CIUDAD, ver todos: Muestra todos los viajes\nViajes a/desde CIUDAD, entre HORA y HORA: Muestra viajes entre el rango horario dado\nGuardar NOMBRE = COMANDO: Almacena temporalmente un comando con el nombre seleccionado para ser ejecutado con el comando mostrar (las variables deben ser cadenas de caracteres en minúscula y/o numeros)\nMostrar NOMBRE: Ejecuta el comando almacenado en NOMBRE\nQueja MENSAJE : En caso de encontrar algun problema con el bot pueden reailizar una queja\n\nEjemplos:\n\"Viajes a Carcarañá, entre 12:00 y 15:30\"\n\"Viajes desde 9 de Julio (BA), ver 5\"\n\"Guardar achabas = Viajes a Chabas\"\n\"Mostrar achabas\"\nRespuesta:\nLos Ranqueles, sale 12:00 y llega 13:00\nDías disponibles: \9989\10062\9989\9989\9989\9989\9989\10062(indicando que no esta disponibles los días Martes y feriados)\n"

tree_ciudades :: BST (Ciudad,Int)
tree_ciudades = fromlistBy ins list_ciudades

bktree_ciudades = fromList $ map (\(w,x)->map toLower w) list_ciudades

list_ciudades::[(Ciudad,Int)]--Lista con todas las ciudades y sus codigos correspodientes
list_ciudades =[("2 de Mayo (MS)",1271),
   ("20 de Septiembre",9801)]

