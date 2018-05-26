{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Control.Monad.IO.Class (liftIO)
import           Eventful.UUID          (uuidNextRandom)
import           Player
import           Servant

type API = "add-new-player" :> ReqBody '[JSON] PlayerData :> Post '[JSON] PlayerId

calcetthonApi :: Server API
calcetthonApi = addNewPlayerHandler

addNewPlayerHandler :: PlayerData -> Handler PlayerId
addNewPlayerHandler playerData = do
    uuid <- liftIO $ uuidNextRandom
    return $ PlayerId uuid
