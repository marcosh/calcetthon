{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class (liftIO)
import           Eventful.Aggregate
import           Eventful.Store.Memory
import           Eventful.UUID          (uuidNextRandom)
import           Player
import           Servant

type API = "add-new-player" :> ReqBody '[JSON] PlayerData :> Post '[JSON] PlayerId

calcetthonApi :: Server API
calcetthonApi = addNewPlayerHandler

addNewPlayerHandler :: PlayerData -> Handler PlayerId
addNewPlayerHandler playerData = do
    uuid <- liftIO uuidNextRandom
    let playerId = PlayerId uuid
    -- this should be done asyncronously
    tvar <- liftIO eventMapTVar
    let
        writer = tvarEventStoreWriter tvar
        reader = tvarEventStoreReader tvar
    let
        streamUuid = read "123e4567-e89b-12d3-a456-426655440000" -- id of the stream of events
    liftIO $ atomically $ commandStoredAggregate writer reader playersAggregate streamUuid $ AddNewPlayer playerId playerData
    -- pure $ addNewPlayer playerId playerData
    -- end of async part
    return $ PlayerId uuid
