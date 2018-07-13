{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Game where

import           Control.Lens ((&), (.~))
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Proxy   (Proxy (Proxy))
import           Data.Swagger
import           Eventful
import           GHC.Generics (Generic)
import           Model.Player
import           Score

newtype GameId = GameId { uuid_ :: UUID }
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data Team = Team
    { defence :: PlayerId
    , attack  :: PlayerId
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data TeamData = TeamData
    { team  :: Team
    , score :: Score
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data GameData = GameData
    { redTeam  :: TeamData
    , blueTeam :: TeamData
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data Game
    = NoGame
    | Game GameId GameData
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

gameCommandHandler :: Game -> GameCommand -> [GameEvent]
gameCommandHandler NoGame (ConcludeGame gameId gameData) = [GameConcluded $ Game gameId gameData]
gameCommandHandler _ (ConcludeGame _ _) = []

-- EVENTS

data GameEvent
    = GameConcluded Game
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

handleGameEvent :: Game -> GameEvent -> Game
handleGameEvent NoGame (GameConcluded game) = game
handleGameEvent game _                      = game

gameProjection :: Projection Game GameEvent
gameProjection = Projection
    { projectionSeed = NoGame
    , projectionEventHandler = handleGameEvent
    }

-- AGGREGATE

gameAggregate :: Aggregate Game GameEvent GameCommand
gameAggregate = Aggregate gameCommandHandler gameProjection
