module Types where 

import Web.Telegram.API.Bot (ChatId)
import Map

newtype Week     = W {runW :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)}
newtype Info     = I {runI :: (Empresa,Time,Time,Week)}
newtype Viaje    = V {runV :: (Ciudad,Ciudad,[Info])}
newtype Empresa  = E {runE :: String} deriving (Ord,Eq)
newtype Ciudad   = City {runCity :: String} deriving (Ord,Eq)
newtype Time     = T {runT :: (Integer,Integer)} deriving (Ord,Eq)
newtype Variable = Var {runVar :: String} deriving (Ord,Eq)
newtype Mensaje  = Msm {runMsm :: (ChatId,String)}
newtype Memoria  = Memo {runMemo :: (Map Integer (Map Variable Comand))}

data Error a = Err String | Result a deriving Show

instance Show Week where
   show (W (lu,ma,mi,ju,vi,sa,dom,fe)) = (f lu)++(f ma)++(f mi)++(f ju)++(f vi)++(f sa)++(f dom)++(f fe)
                                         where f = (\b->if b then "\9989" else "\10062")
instance Show Ciudad where
   show (City c) = c
instance Show Empresa where
   show (E e) = e
instance Show Variable where
   show (Var v) = v
instance Show Mensaje where
   show (Msm m) = show m
instance Show Time where
   show (T (x,y)) = (show x)++":"++(show y)
   

------------------------
---AST de lo Comandos---
------------------------

data Comand = Help
            | Query Bool Ciudad Option
            | Ciudades
            | Let Variable Comand
            | Do Variable
            | Start
            | Complain String
 deriving Show

data Option = Empty
            | All
            | View Int
            | Between Time Time
 deriving Show
