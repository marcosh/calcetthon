{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Game where

import           Model.GameId
import           Model.Team

-- aeson
import           Data.Aeson   (FromJSON, ToJSON)

-- base
import           Data.Proxy   (Proxy (Proxy))
import           GHC.Generics (Generic)

-- lens
import           Control.Lens ((&), (.~))

-- swagger2
import           Data.Swagger

-- eventful-core
import           Eventful

data GameData = GameData
    { redTeam  :: TeamData
    , blueTeam :: TeamData
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

winning :: GameData -> TeamData
winning (GameData red blue) =
    if score red >= score blue
    then red
    else blue

losing :: GameData -> TeamData
losing (GameData red blue) =
    if score red >= score blue
    then blue
    else red

data Game = Game GameId GameData
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToSchema Game where
    declareNamedSchema _ = do
        gameIdSchema <- declareSchemaRef (Proxy :: Proxy GameId)
        gameDataSchema <- declareSchemaRef (Proxy :: Proxy GameData)
        return $ NamedSchema (Just "Game") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("gameId", gameIdSchema)
                , ("gameData", gameDataSchema)
                ]

-- COMMANDS

data GameCommand
    = ConcludeGame GameId GameData

gameCommandHandler :: Maybe Game -> GameCommand -> [GameEvent]
gameCommandHandler Nothing (ConcludeGame gameId gameData) = [GameConcluded $ Game gameId gameData]
gameCommandHandler _ (ConcludeGame _ _) = []

-- EVENTS

data GameEvent
    = GameConcluded Game
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

handleGameEvent :: Maybe Game -> GameEvent -> Maybe Game
handleGameEvent Nothing   (GameConcluded game) = Just game
handleGameEvent maybeGame _                    = maybeGame

gameProjection :: Projection (Maybe Game) GameEvent
gameProjection = Projection
    { projectionSeed = Nothing
    , projectionEventHandler = handleGameEvent
    }

-- AGGREGATE

gameAggregate :: Aggregate (Maybe Game) GameEvent GameCommand
gameAggregate = Aggregate gameCommandHandler gameProjection
