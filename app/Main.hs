{-# LANGUAGE OverloadedStrings #-}

import Data
import Telegram
import Html
import Parser
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.Time (getCurrentTime,Day,diffDays,utctDay)

dias = 5--Cantidad de dias sin consultas(solo valores entre 2 y 6)

newtype Mem m a = M {runM::Memoria->m (a,Memoria)}

instance Monad m => Functor (Mem m) where
    fmap = liftM
 
instance Monad m => Applicative (Mem m) where
    pure   = return
    (<*>)  = ap      

instance Monad m => Monad (Mem m) where
    return x = M (\s -> return (x,s))
    g >>= f  = M (\s-> let t = runM g s in
                        do (n,s')<-t
                           runM (f n) s')

----------------------
---Funciones Monada---
----------------------

save :: Monad m => ChatId -> Variable -> Comand -> Mem m ()--Guarda una variable
save id v c = M (\s->return ((),update id v c s))

load :: Monad m => ChatId->Variable->Mem m (Error Comand)--Busca una variable en memoria
load id v = M (\s->return (lookfor id v s,s))

link :: Monad m => m a -> Mem m a--Me "linkea" las funciones para poder utilizarlas como Mem
link mn = M (\s->do{x<-mn;return (x,s)})

-------------------------
---Cuerpo del Programa---
-------------------------
run_bot :: Manager -> Maybe Int -> Day -> Mem IO ()--Funcion principal del bot. Nunca deberia terminar
run_bot manager lastUpdate d = do day<-link getDay
                                  upd<-link $ receive manager lastUpdate
                                  case upd of
                                    (Err e,l)               -> if diffDays d day < dias
                                                                 then run_bot manager l d
                                                                 else run_bot manager initUpdateId day
                                    (Result (Msm (id,txt)),updid) -> do com<-link $ readComm txt--Parseo los comandos
                                                                        case com of
                                                                         Err e'   -> do{link (send manager (Msm (id,e')));run_bot manager updid day}--Si hubo un error lo comunico,
                                                                         Result x -> do{solveComm prt prq id x;run_bot manager updid day}
                                                                                     where prt = (\s->link $ send manager (Msm (id,s)))
                                                                                           prq = (\s->link $ send manager (Msm (queja_id,s)))
main :: IO ()
main = do manager <- bot_init--Inicializa el bot
          day     <- getDay
          putStr "El bot se ha iniciado correctamente\n"
          runM (run_bot manager initUpdateId day) init_mem--Funcion principal. Se encarga de recibir y responder las consultas
          putStr "Fin del programa\n"--Para el compilador

--------------------------
---Funciones Auxiliares---
--------------------------

solveComm:: (String->Mem IO a) -> (String->Mem IO a) -> ChatId -> Comand -> Mem IO a
solveComm prt prq id Help     = prt mensaje_help
solveComm prt prq id Start    = prt "Bienvenido!!!\nPruebe el comando \"Ayuda\" para mas información (los comandos se escriben sin /)\n"
solveComm prt prq id Ciudades = do prt "Las ciudades disponibles son:\n"
                                   prt $ printCiudades 0 250
                                   prt $ printCiudades 250 500
                                   prt $ printCiudades 500 750
                                   prt $ printCiudades 750 1000
solveComm prt prq id (Let var c)  = save id var c >> prt "Variable guardada exitosamente"
solveComm prt prq id (Complain s) = prq $ "Queja: "++s++"\nAutor: "++show id++"\n"
solveComm prt prq id (Do var)     = do r<-load id var
                                       case r of
                                        Err e    -> prt e
                                        Result c -> solveComm prt prq id c
solveComm prt prq id (Query b ciudad All) = do url<-link $ runH $ make_url b ciudad
                                               case url of
                                                Err e    -> prt e
                                                Result s -> prt $ "Todos los viajes? \128531, ya fue\n"++s
solveComm prt prq id (Query b ciudad opt) = do res<-link $ runH $ consulta ciudad b opt
                                               case res of
                                                Err e    -> prt e
                                                Result x -> imp prt (showQuery x)
                                               where imp p [x]    = p x 
                                                     imp p (x:xs) = p x >> imp p xs

showQuery::Viaje->[String]
showQuery (V (a,b,[])) = ["No hay viajes disponibles desde "++runCity(trcd a)++" a "++runCity(trcd b)++"\n"]
showQuery (V (a,b,xs)) = case showQuery' xs of
                          []     -> ["No hay viajes disponibles desde "++p++" a "++q++"\n"]
                          (x:[]) -> [("Viajes desde "++p++" a "++q++":  \128652\n")++x++("\nBuen Viaje! \128075\128075\128075\n")]
                          (x:xs) -> ["Viajes desde "++p++" a "++q++":  \128652\n"++x]++xs++["\nBuen Viaje! \128075\128075\128075\n"]
                        where p = runCity (trcd a)
                              q = runCity (trcd b)
                              tostr []     = ""
                              tostr (I (e,s,l,w):xs) = "Empresa "++show e++", sale "++show s++" y llega "++show l++"\nDías disponibles:\n"++show w++"\n"++(tostr xs) 
                              showQuery' xs | length xs>45 = tostr (take 45 xs):showQuery' (drop 45 xs)
                                            | otherwise    = [tostr xs]

getDay :: IO Day
getDay = getCurrentTime >>= return.utctDay
