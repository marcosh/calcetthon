module ReadModel.Game where

import           Migrations
import           Model.Game
import           Score

-- eventful-core
import           Eventful               (EventStoreWriter)
import           Eventful.UUID          (UUID)

-- persistent
import           Database.Persist.Class (insert_)
import           Database.Persist.Sql   (SqlPersistT)

gamesProjection :: EventStoreWriter (SqlPersistT IO) GameEvent -> UUID -> GameEvent -> SqlPersistT IO ()
gamesProjection _ _ (GameConcluded (Game gameId gameData)) = insert_ $ Games
    gameId
    (attack     . team  . winning $ gameData)
    (defence    . team  . winning $ gameData)
    (scoreToInt . score . winning $ gameData)
    (attack     . team  . losing  $ gameData)
    (defence    . team  . losing  $ gameData)
    (scoreToInt . score . losing  $ gameData)
