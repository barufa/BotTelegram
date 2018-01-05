{-# LANGUAGE OverloadedStrings #-}

module Telegram(
 bot_init,
 receive,
 send,
 ChatId,
 Manager,
 initUpdateId,
 queja_id
) where--Solo sacar bot_init,receive,send y constructores de tipos

import Data
import Servant.Common.Req
import Network.HTTP.Client
import Web.Telegram.API.Bot
import Network.HTTP.Client.TLS  (tlsManagerSettings)

------------------------
---Parametros del Bot---
------------------------
bot_token = Token (pack "Tu_Token")--Token del bot correspondiente
initUpdateId = Nothing--ID de la actualizacion
limit = Just 1--Cantidad de actualizaciones maxima(dejar en 1, caso contrario modificar run_bot en Main.hs)
timeout = Just 20--Limite en segundo(se queda escuchando hasta que llega una actualizacion, si no llega nada retorna. Solo admite valores <=30)
updateList = ["message"]--Tipos de actualizaciones en las que estoy interesado
queja_id = ChatId 1234--En caso de queja

---------------------------
---Funciones Principales---
---------------------------

bot_init :: IO Manager--Inicia el bot, y retorna un manager (primera funcion a ejecutar)
bot_init = do manager <- newManager tlsManagerSettings
              deleteWebhook bot_token manager
              return manager

receive :: Manager -> Maybe Int -> IO (Error Mensaje,Maybe Int)--Retorna un memsaje con su updateId
receive manager lastUpdate = do t<-getUpdates bot_token lastUpdate limit timeout manager
                                case t of
                                  Left e   -> return (Err ("Sevarnt Error: "++show e++"\n"),lastUpdate)
                                  Right xs -> return (extrac (result xs) lastUpdate)

send :: Manager -> Mensaje -> IO (Error MessageResponse)--Envia un mensaje que se le pasa por argumento
send manager msm = do r<-send' manager msm
                      case r of
                        Right v -> return (Result v)
                        Left e  -> return (Err (show e++"\n"))

--------------------------
---Funciones Auxiliares---
--------------------------

send' :: Manager -> Mensaje -> IO (Either ServantError MessageResponse)
send' manager (chatId,msm) = let request = sendMessageRequest chatId (pack msm)
                              in sendMessage bot_token request manager

extrac :: [Update] -> Maybe Int -> (Error Mensaje,Maybe Int)--Extrae los mensajes de una lista de actualizaciones
extrac []     n = (Err "La lista de actualizaciones es vacia",suc n)
extrac (x:xs) n = case extrac' x of
                    Nothing -> (Err "La actualización recibida no tenia formato de texto",Just (update_id x + 1))
                    Just r  -> (Result r,Just (update_id x + 1))

suc::Maybe Int->Maybe Int
suc Nothing  = Nothing
suc (Just n) = Just (n+1)

extrac' :: Update -> Maybe Mensaje
extrac' upd = do msm<-message upd
                 txt<-text msm
                 return (ChatId (chat_id (chat msm)),unpack txt)
