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
    deriving (Eq, Show, Generic, ToJSON, ToSchema)

data PlayerData = PlayerData
    { name      :: String
    , surname   :: String
    , nickname  :: String
    , birthDate :: Day
    , email     :: EmailAddress
    }
    deriving (Eq, Show, Generic)

instance FromJSON PlayerData

instance ToSchema PlayerData

data Player = Player
    { playerId   :: PlayerId
    , playerData :: PlayerData
    }
    deriving (Eq, Show)

replacePlayer :: Player -> Player -> Player
replacePlayer newPlayer oldPlayer = if playerId oldPlayer == playerId newPlayer then newPlayer else oldPlayer

hasPlayerId :: PlayerId -> Player -> Bool
hasPlayerId pId player = pId == playerId player

newtype Players = Players { list :: [Player] }
    deriving (Eq, Show)

data PlayerEvent
    = AddNewPlayer Player
    | EditPlayer Player
    | RemovePlayer PlayerId

handlePlayerEvent :: Players -> PlayerEvent -> Players
handlePlayerEvent (Players players) (AddNewPlayer player) = Players $ player : players
handlePlayerEvent (Players players) (EditPlayer player) = Players $ map (replacePlayer player) players
handlePlayerEvent (Players players) (RemovePlayer pId) = Players $ filter (not . hasPlayerId pId) players

playersProjection :: Projection Players PlayerEvent
playersProjection = Projection
    { projectionSeed = Players []
    , projectionEventHandler = handlePlayerEvent
    }
