{-# LANGUAGE OverloadedStrings #-}

module Html(
 consulta,
 make_url,
 Handler,
 runH
 ) where

import Data  
import Network.HTTP
import Text.HTML.Tree
import Text.HTML.Parser
import Data.Time           (getZonedTime)
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Prelude hiding      (fail)

type Env = [Token]

newtype Reader a = R { runSt :: Env -> (Error a, Env) }

instance Functor Reader where
    fmap = liftM
 
instance Applicative Reader where
    pure   = return
    (<*>)  = ap      

instance Monad Reader where
    return x = R (\ts -> (Result x,ts))
    g >>= f  = R (\ts-> let (m,ts') = runSt g ts in
                             case m of
                               Err e    -> (Err e,ts')
                               Result v -> runSt (f v) ts')

throw::String->Reader a--Lanza un error
throw s = R (\ts->(Err s,ts))

next :: Reader Token--Retorna el proximo token
next = R (\ts->if length ts==0 then (Err "Next tomo la lista vacía",ts) else (Result (head ts),tail ts))

try :: Reader a -> Reader (Either String a)--Trata de ejecutar la funcion f capturando el error
try f = R (\ts->let (m,s)=runSt f ts in case m of
                      Err e   ->(Result (Left e),ts)
                      Result v->(Result (Right v),s))

search :: Text -> Text -> Text -> Reader Token--Busca con una etiqueta particular y atributos particulares, devuelve el token
search t a v = R (\ts->case ts of
                        (x:e:xs) -> if isO x t a v then (Result x,e:xs) else runSt (search t a v) (e:xs)
                        _        -> (Err ("No existe "++(unpack t)++","++(unpack a)++","++(unpack v)++"\n"),ts))

------------------------------------------

newtype Handler a = H {runH :: IO (Error a)}

instance Functor Handler where
    fmap = liftM
 
instance Applicative Handler where
    pure   = return
    (<*>)  = ap      

instance Monad Handler where
    return x = H (return (Result x))
    g >>= f  = H (do r<-runH g
                     case r of
                       Err e    -> return (Err e)
                       Result x -> runH (f x))

fail :: String -> Handler a
fail s = H (return (Err s))

link :: IO a -> Handler a
link f = H (do{x<-f;return (Result x)})

------------------------------------------

totime :: String -> Maybe Time
totime (' ':xs)             = totime xs
totime (x1:x2:':':x4:x5:xs) = Just $ T (read [x1,x2]::Integer,read [x4,x5]::Integer)
totime xs                   = Nothing

findEmpresa :: Reader Empresa
findEmpresa = do search (pack "a") (pack "title") (pack "Datos de la empresa")
                 txt<-next
                 case txt of
                   ContentText t -> return $ E (unpack t)
                   _             -> throw "La etiqueta encontrada en findEmpresa no tiene formato texto"

findSale :: Reader Time
findSale = do search (pack "td") (pack "class") (pack "sale")
              txt<-next
              case txt of
                ContentText t -> case totime (unpack t) of
                                  Just x  -> return $ x
                                  Nothing -> throw "Error al buscar el horario"
                _             -> throw "La etiqueta encontrada en findSale no tiene formato texto"

findLlega :: Reader Time
findLlega = do search (pack "td") (pack "class") (pack "sale")
               txt<-next
               case txt of
                 ContentText t -> case totime (unpack t) of
                                   Just x  -> return $ x
                                   Nothing -> throw "Error al buscar el horario"
                 _             -> throw "La etiqueta encontrada en findLlega no tiene formato texto"

findDia :: Reader Bool
findDia = do tk<-search (pack "img") (pack "class") (pack "yes-no")
             case extracAttr tk (pack "src") of
               Nothing -> throw "El toquen encontrado en findDia no contine la etiqueta src"
               Just s  -> return (s==icon_yes)
             where attr=pack "src";icon_yes=pack "http://www.terminalrosario.gob.ar/wp-content/themes/terminal-rosario/images/table-icon-yes.png"
                                                 --Formato de los iconos tilde

findWeek :: Reader Week--Trata de armar una semana, falla si no encuentra los 7 dias + feriado
findWeek = do lun<-findDia
              mar<-findDia
              mie<-findDia
              jue<-findDia
              vie<-findDia
              sab<-findDia
              dom<-findDia
              fer<-findDia
              return $ W (lun,mar,mie,jue,vie,sab,dom,fer)

findViaje :: Reader Info--Parsea un viaje
findViaje = do emp  <-findEmpresa
               sal  <-findSale
               lle  <-findLlega
               week <-findWeek
               return $ I (emp,sal,lle,week)

findAll::Reader [Info]--Dado un estado busca todos los viajes y falla si no encuentra nada
findAll = do v<-findViaje
             e<-(try findAll)
             case e of
               Right vs -> return (v:vs)
               Left e   -> return [v]

extracHtml::Env->Handler [Info]--Busca los viajes
extracHtml ts = case fst (runSt findAll ts) of
                  Result x -> return x
                  Err e    -> fail "Error al buscar la información solicitada, es posible que no haya viajes disponibles\n"
                              --Error que receibe el usuario

------------------------------------------------

parserHtml::String->IO Env--Parsea el html y me lo devuelve como Env([Token])
parserHtml page = return (parseTokens (pack page))

get :: String -> IO String--Toma una url y me devuelve el html como string
get url = simpleHTTP (getRequest url) >>= getResponseBody

-----------------------
---Funcion Principal---
-----------------------

consulta :: Ciudad->Bool->Option->Handler Viaje--Funcion Principal, se encarga de responder una consulta dado el comando correspondiente
consulta c f Empty         = consulta_n c f 3
consulta c f All           = consulta_all c f
consulta c f (View n)      = consulta_n c f n
consulta c f (Between l r) = consulta_entre c f l r

consulta_all::Ciudad->Bool->Handler Viaje--Funcion auxiliar, devuelve todos los viajes
consulta_all ciudad from = do url  <- make_url from ciudad
                              html <- link (get url)
                              page <- link (parserHtml html)
                              xs   <- extracHtml page
                              return $ if from then V (City "Rosario",ciudad,xs) else V (ciudad,City "Rosario",xs) 
                                                 
consulta_entre::Ciudad->Bool->Time->Time->Handler Viaje--Devuelve viajes entre horarios
consulta_entre ciudad from l r | l>r || l>=(T (24,00)) || r>=(T (24,00))  = fail "El rango horario es inconsistente"
                               | l<=r                                     = do V (x,y,xs)<-consulta_all ciudad from
                                                                               return $ V (x,y,filter cmp xs)
                                                                            where cmp=(\v->let t=time v in l<=t && t<=r)
                                                                                  time (I (e,s,l,w)) = s

consulta_n::Ciudad->Bool->Int->Handler Viaje--Devuelve los proximos n viajes
consulta_n ciudad from n = do h          <- getTime
                              V (x,y,xs) <- consulta_all ciudad from
                              let time (I (e,s,l,w)) = s
                                  cmp=(\v->let t=time v in h<=t)
                                  rs = (filter cmp xs)++(filter (not.cmp) xs)
                               in return $ V (x,y,take n rs)

--------------------------
---Funciones Auxiliares---
--------------------------

isO :: Token -> Text -> Text -> Text -> Bool--Si abre una etiqueta con Tag y contine el atributo [attr = val]
isO (TagOpen y ys) tag attr val = y==tag && (isOn ys attr val)
isO  token         tag attr val = False

isOn :: [Attr] -> Text -> Text -> Bool --Si en la lista aprece el atributo s con el valor p
isOn []                     s p = False
isOn ((Attr name value):xs) s p = (name==s && value==p) || (isOn xs s p)  

extracAttr :: Token -> Text -> Maybe Text --Extrae de un token el valor de un atributo
extracAttr tag s = case tag of
                     (TagOpen y ys)      -> extracAttr' ys s 
                     (TagSelfClose y ys) -> extracAttr' ys s 
                     _                   -> Nothing 
                   where extracAttr' [] s                                 = Nothing
                         extracAttr' ((Attr name value):xs) s | name==s   = Just value
                                                              | otherwise = extracAttr' xs s

make_url::Bool->Ciudad-> Handler String--Dada una ciudad y un boleano(si es desde rosario o no) me crea la url para conseguir la informacion
make_url from ciudad = case codigo ciudad of
                          Nothing -> fail $ "La ciudad ingresada no es valida\n"++perhaps ciudad
                          Just n  -> if from then make_url' 2000 n else  make_url' n 2000--2000 es el codigo de rosario
                       where make_url' n m = return $ "http://www.terminalrosario.gob.ar/buscador/"++show n++"/"++show m++"/"

getTime::Handler Time--Obtiene la hora del sistema
getTime = do t  <- link (getZonedTime)
             t' <- link (timed (show t))
             case t' of
               Just h  -> case totime h of 
                            Just x  -> return $ x
                            Nothing -> fail "Error al leer la hora del sistema"
               Nothing -> fail "Error al leer la hora del sistema"
           where timed (h1:h2:c:m1:m2:xs) | c==':'    = return (Just [h1,h2,c,m1,m2])
                                          | otherwise = timed (h2:c:m1:m2:xs)
                 timed _                              = return Nothing

