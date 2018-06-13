{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Player where

import           Control.Lens
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.String        ()
import           Data.Swagger
import           Data.Time.Calendar (Day)
import           Eventful
import           GHC.Generics       (Generic)
import           Text.EmailAddress  (EmailAddress)

instance ToSchema EmailAddress where
    declareNamedSchema _ = return $ NamedSchema (Just "email") $ mempty
        & type_ .~ SwaggerString
        & description ?~ "email"
        & format ?~ "email"
        & example ?~ "gigi@zucon.com"

newtype PlayerId = PlayerId { uuid :: UUID }
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data PlayerData = PlayerData
    { name      :: String
    , surname   :: String
    , nickname  :: String
    , birthDate :: Day
    , email     :: EmailAddress
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data Player = Player
    { playerId   :: PlayerId
    , playerData :: PlayerData
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

replacePlayer :: Player -> Player -> Player
replacePlayer newPlayer oldPlayer = if playerId oldPlayer == playerId newPlayer then newPlayer else oldPlayer

hasPlayerId :: PlayerId -> Player -> Bool
hasPlayerId pId player = pId == playerId player

newtype Players = Players { list :: [Player] }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

-- COMMANDS

data PlayerCommand
    = AddNewPlayer PlayerId PlayerData
    | RemovePlayer PlayerId
    | EditPlayer PlayerId PlayerData

playerCommandHandler :: Players -> PlayerCommand -> [PlayerEvent]
playerCommandHandler players (AddNewPlayer playerId playerData) = addNewPlayer players playerId playerData
playerCommandHandler players (RemovePlayer playerId) = removePlayer players playerId
playerCommandHandler players (EditPlayer playerId playerData)   = editPlayer players playerId playerData

addNewPlayer :: Players -> PlayerId -> PlayerData -> [PlayerEvent]
addNewPlayer _ playerId playerData = [NewPlayerAdded $ Player playerId playerData]

removePlayer :: Players -> PlayerId -> [PlayerEvent]
removePlayer _ playerId = [PlayerRemoved playerId]

editPlayer :: Players -> PlayerId -> PlayerData -> [PlayerEvent]
editPlayer _ playerId playerData = [PlayerEdited $ Player playerId playerData]

-- EVENTS

data PlayerEvent
    = NewPlayerAdded Player
    | PlayerEdited Player
    | PlayerRemoved PlayerId
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

handlePlayerEvent :: Players -> PlayerEvent -> Players
handlePlayerEvent (Players players) (NewPlayerAdded player) = Players $ player : players
handlePlayerEvent (Players players) (PlayerEdited player) = Players $ map (replacePlayer player) players
handlePlayerEvent (Players players) (PlayerRemoved pId) = Players $ filter (not . hasPlayerId pId) players

playersProjection :: Projection Players PlayerEvent
playersProjection = Projection
    { projectionSeed = Players []
    , projectionEventHandler = handlePlayerEvent
    }

-- AGGREGATE

playersAggregate :: Aggregate Players PlayerEvent PlayerCommand
playersAggregate = Aggregate playerCommandHandler playersProjection
