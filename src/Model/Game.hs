{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Game where

import           Model.GameId
import           Model.PlayerId
import           Model.TeamScore

-- aeson
import           Data.Aeson      (FromJSON, ToJSON)

-- base
import           Data.Proxy      (Proxy (Proxy))
import           GHC.Generics    (Generic)

-- lens
import           Control.Lens    ((&), (.~))

-- swagger2
import           Data.Swagger

-- eventful-core
import           Eventful

data GameData player = GameData
    { redTeam  :: TeamScore player
    , blueTeam :: TeamScore player
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data Game player = Game GameId (GameData player)
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToSchema (Game player) where
    declareNamedSchema _ = do
        gameIdSchema <- declareSchemaRef (Proxy :: Proxy GameId)
        gameDataSchema <- declareSchemaRef (Proxy :: Proxy (GameData player))
        return $ NamedSchema (Just "Game") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("gameId", gameIdSchema)
                , ("gameData", gameDataSchema)
                ]

-- COMMANDS

data GameCommand
    = ConcludeGame GameId (GameData PlayerId)

gameCommandHandler :: Maybe (Game PlayerId) -> GameCommand -> [GameEvent]
gameCommandHandler Nothing (ConcludeGame gameId gameData) = [GameConcluded $ Game gameId gameData]
gameCommandHandler _ (ConcludeGame _ _) = []

-- EVENTS

data GameEvent
    = GameConcluded (Game PlayerId)
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

handleGameEvent :: Maybe (Game PlayerId) -> GameEvent -> Maybe (Game PlayerId)
handleGameEvent Nothing   (GameConcluded game) = Just game
handleGameEvent maybeGame _                    = maybeGame

gameProjection :: Projection (Maybe (Game PlayerId)) GameEvent
gameProjection = Projection
    { projectionSeed = Nothing
    , projectionEventHandler = handleGameEvent
    }

-- AGGREGATE

gameAggregate :: Aggregate (Maybe (Game PlayerId)) GameEvent GameCommand
gameAggregate = Aggregate gameCommandHandler gameProjection
