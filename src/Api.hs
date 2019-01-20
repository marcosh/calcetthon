{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Api where

import           Model.Game
import           Model.Model
import           Model.Player
import           Model.PlayerData
import           Model.PlayerId

import           ReadModel.Player            as RMPlayers (allPlayers,
                                                           playersProjection)
-- import           ProcessManager              (processManager)

-- aeson
import           Data.Aeson                  (ToJSON)
import           Data.Aeson.Encode.Pretty

-- bytestring
import qualified Data.ByteString.Lazy.Char8  as BSL

-- contravariant
import           Data.Functor.Contravariant  (contramap)

-- eventful-core
import           Eventful
import           Eventful.UUID               (uuidNextRandom)

-- eventful-postgresql
import           Eventful.Store.Postgresql

-- monad-logger
import           Control.Monad.Logger        (runStderrLoggingT)

-- resource-pool
import           Data.Pool                   (Pool)

-- transformers
import           Control.Monad.IO.Class      (liftIO)

-- persistent
import           Database.Persist.Sql

-- persistent-postgresql
import           Database.Persist.Postgresql (createPostgresqlPool)

-- servant-server
import           Servant

type API = "add-new-player" :> ReqBody '[JSON] PlayerData :> Post '[JSON] Player
    :<|> "players" :> Get '[JSON] Players
    :<|> "record-game" :> ReqBody '[JSON] GameData :> Post '[JSON] GameId
    :<|> "game" :> Capture "gameId" UUID :> Get '[JSON] Game

calcetthonApi :: Server API
calcetthonApi = addNewPlayerHandler
    :<|> playersHandler
    :<|> recordGameHandler
    :<|> gameHandler

reader :: VersionedEventStoreReader (SqlPersistT IO) CalcetthonEvent
reader = serializedVersionedEventStoreReader jsonStringSerializer $ sqlEventStoreReader defaultSqlEventStoreConfig

-- globalReader :: (FromJSON a, ToJSON a) => GlobalEventStoreReader (SqlPersistT IO) a
-- globalReader = serializedGlobalEventStoreReader jsonStringSerializer $ sqlGlobalEventStoreReader defaultSqlEventStoreConfig

writer :: EventStoreWriter (SqlPersistT IO) CalcetthonEvent
writer = synchronousEventBusWrapper
    (serializedEventStoreWriter jsonStringSerializer $ postgresqlEventStoreWriter defaultSqlEventStoreConfig)
    -- [processManagerHandler]
    [ eventPrinter
    , readPlayersProjection
    ]

eventPrinter :: EventStoreWriter (SqlPersistT IO) CalcetthonEvent -> UUID -> CalcetthonEvent -> SqlPersistT IO ()
eventPrinter _ uuid event = liftIO $ printJSONPretty (uuid, event)

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' defConfig

readPlayersProjection :: EventStoreWriter (SqlPersistT IO) CalcetthonEvent -> UUID -> CalcetthonEvent -> SqlPersistT IO ()
readPlayersProjection eventStoreWriter uuid (CalcetthonPlayerEvent playerEvent) = RMPlayers.playersProjection (contramap CalcetthonPlayerEvent eventStoreWriter) uuid playerEvent
readPlayersProjection _                _    _                                   = pure ()

-- processManagerHandler :: EventStoreWriter (SqlPersistT IO) CalcetthonEvent -> UUID -> CalcetthonEvent -> SqlPersistT IO ()
-- processManagerHandler pmWriter _ _ =
--     -- streamProjection <- getLatestStreamProjection globalReader (globalStreamProjection $ processManagerProjection processManager)
--     applyProcessManagerCommandsAndEvents processManager pmWriter reader ()

-- PLAYERS

pool :: IO (Pool SqlBackend)
pool = runStderrLoggingT $ createPostgresqlPool conn 10 where
    conn = "host=localhost dbname=calcetthon user=calcetthon password=calcetthon port=5432"

addNewPlayerHandler :: PlayerData -> Handler Player
addNewPlayerHandler playerData = do
    uuid <- liftIO uuidNextRandom
    let
        playerId = PlayerId uuid
        -- streamUuid = read "123e4567-e89b-12d3-a456-426655440000" -- id of the stream of events
    maybePlayerDataHashed <- liftIO $ hashPassword playerData
    case maybePlayerDataHashed of
        Nothing               -> throwError $ err500 { errBody = "unable to hash the provided password" }
        Just playerDataHashed -> do
    -- this should be done asyncronously
            _ <- liftIO $ runSqlPool
                (commandStoredAggregate writer reader calcetthonPlayersAggregate uuid $ CalcetthonPlayerCommand $ AddNewPlayer playerId playerDataHashed) =<< pool
    --- end of async part
            return $ Player (PlayerId uuid) playerData

playersHandler :: Handler Players
playersHandler = liftIO $ runSqlPool allPlayers =<< pool
    -- let uuid = read "123e4567-e89b-12d3-a456-426655440000"
    -- events <- liftIO $ runSqlPool (getEvents reader (allEvents uuid)) =<< pool
    -- return $ latestProjection calcetthonPlayersProjection $ fmap streamEventEvent events

-- GAME

recordGameHandler :: GameData -> Handler GameId
recordGameHandler gameData = do
    uuid <- liftIO uuidNextRandom
    let
        gameId = GameId uuid
        -- streamUuid = read "ae55b01f-ece8-40d5-acc0-fc5afefda9f1"
    -- this should be done asynchronously
    _ <- liftIO $ runSqlPool (commandStoredAggregate writer reader calcetthonGameAggregate uuid $ CalcetthonGameCommand $ ConcludeGame gameId gameData) =<< pool
    -- end of async part
    return $ GameId uuid

gameHandler :: UUID -> Handler Game
gameHandler gameId = do
    --let streamUuid = read "ae55b01f-ece8-40d5-acc0-fc5afefda9f1"
    events <- liftIO $ runSqlPool (getEvents reader (allEvents gameId)) =<< pool
    return $ latestProjection calcetthonGameProjection $ fmap streamEventEvent events
