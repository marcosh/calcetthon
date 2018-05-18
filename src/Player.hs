module Player where

import           Data.String        (IsString)
import           Data.Time.Calendar (Day)
import           Eventful
import           Text.Email.Parser  (EmailAddress)

newtype PlayerId = PlayerId { uuid :: UUID }
    deriving (Eq, Show)

data Player = Player
    { playerId  :: PlayerId
    , name      :: String
    , surname   :: String
    , nickname  :: String
    , birthDate :: Day
    , email     :: EmailAddress
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
