{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Control.Monad.Reader        ()
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Pool                   (Pool)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql
import           Eventful
import           Eventful.Store.Postgresql
import           Eventful.UUID               (uuidNextRandom)
import           Game
import           Player
import           Servant

type API = "add-new-player" :> ReqBody '[JSON] PlayerData :> Post '[JSON] PlayerId
    :<|> "players" :> Get '[JSON] Players
    :<|> "record-game" :> ReqBody '[JSON] GameData :> Post '[JSON] GameId
    :<|> "game" :> Capture "gameId" UUID :> Get '[JSON] Game

calcetthonApi :: Server API
calcetthonApi = addNewPlayerHandler
    :<|> playersHandler
    :<|> recordGameHandler
    :<|> gameHandler

reader :: (FromJSON a, ToJSON a) => VersionedEventStoreReader (SqlPersistT IO) a
reader = serializedVersionedEventStoreReader jsonStringSerializer $ sqlEventStoreReader defaultSqlEventStoreConfig

writer ::(FromJSON a, ToJSON a) => EventStoreWriter (SqlPersistT IO) a
writer = serializedEventStoreWriter jsonStringSerializer $ postgresqlEventStoreWriter defaultSqlEventStoreConfig

-- PLAYERS

pool :: IO (Pool SqlBackend)
pool = runStderrLoggingT $ createPostgresqlPool conn 10 where
    conn = "host=localhost dbname=calcetthon user=calcetthon password=calcetthon port=5432"

addNewPlayerHandler :: PlayerData -> Handler PlayerId
addNewPlayerHandler playerData = do
    uuid <- liftIO uuidNextRandom
    let
        playerId = PlayerId uuid
        streamUuid = read "123e4567-e89b-12d3-a456-426655440000" -- id of the stream of events
    -- this should be done asyncronously
    liftIO $ runSqlPool (commandStoredAggregate writer reader playersAggregate streamUuid $ AddNewPlayer playerId playerData) =<< pool
    --- end of async part
    return $ PlayerId uuid

playersHandler :: Handler Players
playersHandler = do
    let uuid = read "123e4567-e89b-12d3-a456-426655440000"
    events <- liftIO $ runSqlPool (getEvents reader (allEvents uuid)) =<< pool
    return $ latestProjection playersProjection $ fmap streamEventEvent events

-- GAME

recordGameHandler :: GameData -> Handler GameId
recordGameHandler gameData = do
    uuid <- liftIO uuidNextRandom
    let
        gameId = GameId uuid
        -- streamUuid = read "ae55b01f-ece8-40d5-acc0-fc5afefda9f1"
    -- this should be done asynchronously
    liftIO $ runSqlPool (commandStoredAggregate writer reader gameAggregate uuid $ ConcludeGame gameId gameData) =<< pool
    -- end of async part
    return $ GameId uuid

gameHandler :: UUID -> Handler Game
gameHandler gameId = do
    --let streamUuid = read "ae55b01f-ece8-40d5-acc0-fc5afefda9f1"
    events <- liftIO $ runSqlPool (getEvents reader (allEvents gameId)) =<< pool
    return $ latestProjection gameProjection $ fmap streamEventEvent events
