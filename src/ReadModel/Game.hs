module ReadModel.Game where

import           Migrations
import           Model.Game
import           Model.GameId
import           Model.Player           hiding (Players)
import           Model.PlayerId
import           Model.Team
import           Model.TeamScore
import           ReadModel.Player
import           Score

-- base
import           Data.Maybe             (catMaybes, listToMaybe)

-- esqueleto
import           Database.Esqueleto

-- eventful-core
import           Eventful               (EventStoreWriter)
import           Eventful.UUID          (UUID)

-- persistent
import           Database.Persist.Class (getBy, insert_)
import           Database.Persist.Sql   (SqlPersistT)
import           Database.Persist.Types (Entity)

-- transformers
import           Control.Monad.IO.Class (liftIO)

gamesProjection :: EventStoreWriter (SqlPersistT IO) GameEvent -> UUID -> GameEvent -> SqlPersistT IO ()
gamesProjection _ _ (GameConcluded (Game gameId gameData)) = insert_ $ Games
    gameId
    (attack     . team  . redTeam  $ gameData)
    (defence    . team  . redTeam  $ gameData)
    (scoreToInt . score . redTeam  $ gameData)
    (attack     . team  . blueTeam $ gameData)
    (defence    . team  . blueTeam $ gameData)
    (scoreToInt . score . blueTeam $ gameData)

buildGameFromDb :: (Entity Games, Entity Players, Entity Players, Entity Players, Entity Players) -> Maybe (Game Player)
buildGameFromDb (game, redAttack, redDefence, blueAttack, blueDefence) = do
    redScore  <- (intToScore . gamesRedScore  . entityVal) game
    blueScore <- (intToScore . gamesBlueScore . entityVal) game
    return $ Game
        (gamesUuid $ entityVal game)
        (GameData
            (TeamScore
                (Team (buildPlayerFromDb redAttack) (buildPlayerFromDb redDefence))
                redScore
            )
            (TeamScore
                (Team (buildPlayerFromDb blueAttack) (buildPlayerFromDb blueDefence))
                blueScore
            )
        )

gameById :: GameId -> SqlPersistT IO (Maybe (Game Player))
gameById gameId = do
    games <- select $
            from $ \(game, redAttack, redDefence, blueAttack, blueDefence) -> do
                where_ $ (game ^. GamesUuid ==. val gameId)
                    &&. (game ^. GamesRedAttack ==. redAttack ^. PlayersUuid)
                    &&. (game ^. GamesRedDefence ==. redDefence ^. PlayersUuid)
                    &&. (game ^. GamesBlueAttack ==. blueAttack ^. PlayersUuid)
                    &&. (game ^. GamesBlueDefence ==. blueDefence ^. PlayersUuid)
                return (game, redAttack, redDefence, blueAttack, blueDefence)
    return $ listToMaybe $ catMaybes $ buildGameFromDb <$> games
