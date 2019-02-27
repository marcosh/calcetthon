module ReadModel.Player where

import           Migrations
import           Model.Email
import           Model.Player           hiding (Players)
import qualified Model.Player           as P (Players (..))
import           Model.PlayerData
import           Model.PlayerId

-- eventful-core
import           Eventful               (EventStoreWriter)
import           Eventful.UUID          (UUID)

-- persistent
import           Database.Persist.Class (insert_, selectList)
import           Database.Persist.Sql   (SqlPersistT)
import           Database.Persist.Types (Entity, entityVal)

playersProjection :: EventStoreWriter (SqlPersistT IO) PlayerEvent -> UUID -> PlayerEvent -> SqlPersistT IO ()
playersProjection _ _ (NewPlayerAdded player) = insert_ $ Players
    (playerUuid      player)
    (playerName      player)
    (playerSurname   player)
    (playerNickname  player)
    (playerBirthDate player)
    (emailAddress_ $ playerEmail player)
    (playerPassword  player)
playersProjection _ _ _                  = pure ()

buildPlayerFromDb :: Entity Players -> Player
buildPlayerFromDb dbPlayer =
    let
        entityPlayer = entityVal dbPlayer
    in
        Player
            (playersUuid entityPlayer)
            (PlayerData
                (playersName      entityPlayer)
                (playersSurname   entityPlayer)
                (playersNickname  entityPlayer)
                (playersBirthDate entityPlayer)
                (Email $ playersEmail     entityPlayer)
                (playersPassword  entityPlayer)
            )

allPlayers :: SqlPersistT IO P.Players
allPlayers = do
    entityPlayers <- selectList [] []
    pure $ P.Players $ buildPlayerFromDb <$> entityPlayers
