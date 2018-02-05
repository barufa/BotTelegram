{-# LANGUAGE OverloadedStrings #-}

module Parser(readComm) where

import Data
import Html
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Char (isAlphaNum)

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

newtype Parserror a = P (Parser (Error a))

runP :: Parserror a -> Parser (Error a)
runP (P p) = p

instance Functor Parserror where
    fmap = liftM
 
instance Applicative Parserror where
    pure   = return
    (<*>)  = ap      

instance Monad Parserror where
    return x = P (return (Result x))
    g >>= f  = P (do x<-runP g
                     case x of
                       Err e    -> return (Err e)
                       Result z -> runP (f z))

link :: Parser a -> Parserror a
link p = P (p >>= (\x->return (Result x)))

throw :: String -> Parserror a
throw s = P (return (Err s))

totParser :: Parserror a -> Parser (Error a)
totParser p = do whiteSpace lis
                 t <- runP p
                 case t of 
                  Err e    -> return $ Err e
                  Result x -> eof >> return (Result x)

--Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef{reservedNames=["ayuda","viajes","ciudades","guardar","ver","show","mostrar","entre","queja"]})

------------------------
---Parser de Comandos---
------------------------

---Comando Help---
parserHelp :: Parserror Comand--Se encarga del comando Help
parserHelp = link (reserved lis "ayuda") >> return Help

---Comando Queja---
parserQueja :: Parserror Comand--Se encarga del comando Help
parserQueja = link (reserved lis "queja" >> manyTill anyChar (try eof)) >>= return.Complain

---Comando Ciudad---
parserCiudades :: Parserror Comand--Se encarga del comando ciudades
parserCiudades = link (reserved lis "ciudades") >> return Ciudades

---Comando Start---
parserStart :: Parserror Comand--Se encarga del comando ciudades
parserStart = link (reserved lis "/start") >> return Start

---Comando Viajes---
parserViajes :: Parserror Comand--Se encarga del comando viajes
parserViajes = do link $ reserved lis "viajes"
                  b  <- link $  try (symbol lis "desde") <|> (symbol lis "a")
                  c  <- parserciudad
                  op <- parserOption
                  return $ Query (b=="a") c op
                    
parserOption :: Parserror Option--Se encarga de parsear las opciones
parserOption = link $ try parseroption_n <|> (try parseroption_t <|> (try parseroption_a <|> return Empty))

parseroption_a :: Parser Option--Aux option
parseroption_a = do symbol lis ","
                    reserved lis "ver"
                    symbol lis  "todos"
                    return All

parseroption_n :: Parser Option--Aux option
parseroption_n = do symbol lis ","
                    reserved lis "ver"
                    n<-natural lis
                    return $ View $ fromIntegral n

parseroption_t :: Parser Option--Aux option
parseroption_t = do symbol lis ","
                    reserved lis "entre"
                    a<-natural lis
                    symbol lis  ":"
                    b<-natural lis
                    symbol lis  "y"
                    c<-natural lis
                    symbol lis  ":"
                    d<-natural lis
                    return $ Between (T (a,b)) (T (c,d))
                              
parserciudad :: Parserror Ciudad--Parsea una ciudad
parserciudad = do c<-link $ many $ satisfy (\c->isAlphaNum c || c=='(' || c==')' || c==' ' || c=='.')
                  case fix c of
                   [] -> fail "Error al leer la ciudad"
                   ys -> let xs=City ys in if isCiudad xs then return xs else throw $ "La ciudad ingresada no es valida\n"++perhaps xs

---Comando Let---
parserLet :: Parserror Comand--Se encarga del comando Let
parserLet = do link $ reserved lis "guardar"
               v<-parserVar
               link $ symbol lis  "="
               cm<-parserComm'
               return $ Let v cm

parserVar::Parserror Variable--Parsea una variable
parserVar = link $ identifier lis >>= return.Var

---Comando Do---
parserDo :: Parserror Comand--Se encarga del comando Mostrar
parserDo = do link $ reserved lis "mostrar"
              v<-parserVar
              return (Do v)

-----------------------
---Funcion Principal---
-----------------------
--Se encarga de parsear la string y devolver los comando que esta almacena(funcion a modo de interfaz)
readComm :: String -> IO (Error Comand)
readComm s = case parserComm s of
                  Left e  -> return $ Err "Error, el comando ingresado no es valido."
                  Right c -> return c
--------------------------
---Funciones Auxiliares---
--------------------------

parserComm' :: Parserror Comand--Se encarga de parsear los comandos
parserComm' = let help   = runP parserHelp
                  ciudad = runP parserCiudades
                  viajes = runP parserViajes
                  clet   = runP parserLet
                  ver    = runP parserDo
                  str    = runP parserStart
                  queja  = runP parserQueja
              in P $ choice [help,ciudad,viajes,clet,ver,str,queja]

parserComm :: String -> Either ParseError (Error Comand)--Dado una String, parsea y devuelve el comando leido o un error
parserComm = (parse (totParser parserComm') "").(map toLower)

fix :: String -> String
fix c = reverse $ fx' $ reverse $ fx' $ tr c
        where fx' [] = []
              fx' (x:xs) = if x==' ' then fx' xs else x:xs
              tr xs | length xs == 0 = []
                    | length xs == 1 = if head xs == ' ' then [] else xs
                    | otherwise      = let x  = head xs 
                                           y  = head $ tail xs
                                           ys = tr $ tail xs
                                        in if x==' ' && y==' ' then ys else x:ys
