module Types where 

import Web.Telegram.API.Bot (ChatId)
import Map

type Week     = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)
type Info     = (Empresa,Time,Time,Week)
type Viaje    = (Ciudad,Ciudad,[Info])
type Empresa  = String
type Ciudad   = String
type Time     = String
type Variable = String
type Memoria  = Map Integer (Map Variable Comand)
type Mensaje  = (ChatId,String)

data Error a = Err String | Result a deriving Show

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
