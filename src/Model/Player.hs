{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Player where

import           Model.Email
import           Model.PlayerData
import           Model.PlayerId

-- aeson
import           Data.Aeson         (FromJSON, ToJSON, object, toJSON, (.=))

-- base
import           Data.Proxy         (Proxy (Proxy))
import           GHC.Generics       (Generic)

-- eventful-core
import           Eventful           (Aggregate (Aggregate),
                                     Projection (Projection),
                                     projectionEventHandler, projectionSeed)

-- lens
import           Control.Lens       ((&), (.~), (?~))

-- swagger2
import           Data.Swagger       (NamedSchema (NamedSchema),
                                     SwaggerType (SwaggerObject), ToSchema,
                                     declareNamedSchema, declareSchemaRef,
                                     description, properties, type_)

-- time
import           Data.Time.Calendar (Day)

data Player = Player
    { playerId_   :: PlayerId
    , playerData_ :: PlayerData
    }
    deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Player where
    toJSON (Player playerId playerData) = object
        [ "id"        .= playerId
        , "email"     .= email playerData
        , "name"      .= name playerData
        , "surname"   .= surname playerData
        , "nickname"  .= nickname playerData
        , "birthDate" .= birthDate playerData
        ]

instance ToSchema Player where
    declareNamedSchema _ = do
        idSchema     <- declareSchemaRef (Proxy :: Proxy PlayerId)
        stringSchema <- declareSchemaRef (Proxy :: Proxy String  )
        daySchema    <- declareSchemaRef (Proxy :: Proxy Day     )
        emailSchema  <- declareSchemaRef (Proxy :: Proxy Email   )
        return $ NamedSchema (Just "player") $ mempty
            & type_ .~ SwaggerObject
            & description ?~ "player"
            & properties .~
                [ ("id"       , idSchema    )
                , ("name"     , stringSchema)
                , ("surname"  , stringSchema)
                , ("nickname" , stringSchema)
                , ("birthDate", daySchema   )
                , ("email"    , emailSchema )
                ]

playerUuid :: Player -> PlayerId
playerUuid (Player playerId _) = playerId

playerName :: Player -> String
playerName (Player _ playerData) = name playerData

playerSurname :: Player -> String
playerSurname (Player _ playerData) = surname playerData

playerNickname :: Player -> String
playerNickname (Player _ playerData) = nickname playerData

playerBirthDate :: Player -> Day
playerBirthDate (Player _ playerData) = birthDate playerData

playerEmail :: Player -> Email
playerEmail (Player _ playerData) = email playerData

replacePlayer :: Player -> Player -> Player
replacePlayer newPlayer oldPlayer = if playerId_ oldPlayer == playerId_ newPlayer then newPlayer else oldPlayer

hasPlayerId :: PlayerId -> Player -> Bool
hasPlayerId pId player = pId == playerId_ player

newtype Players = Players { list :: [Player] }
    deriving (Eq, Show, Generic, ToJSON, ToSchema)

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
