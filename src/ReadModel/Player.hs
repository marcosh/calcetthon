module ReadModel.Player where

import           Migrations
import           Model.Email
import           Model.Player           hiding (Players)
import           Model.PlayerId

-- eventful-core
import           Eventful               (EventStoreWriter)
import           Eventful.UUID          (UUID, uuidToText)

-- persistent
import           Database.Persist.Class (insert_)
import           Database.Persist.Sql   (SqlPersistT)

-- text
import           Data.Text              (unpack)

playersProjection :: EventStoreWriter (SqlPersistT IO) PlayerEvent -> UUID -> PlayerEvent -> SqlPersistT IO ()
playersProjection _ _ (NewPlayerAdded player) = insert_ $ Players
    (unpack $ uuidToText (uuid_ $ playerUuid player))
    (playerName      player)
    (playerSurname   player)
    (playerNickname  player)
    (playerBirthDate player)
    (emailAddress_ $ playerEmail player)
playersProjection _ _ _                  = pure ()
