{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EventStore where

import           Control.Concurrent.STM
import           Data.Maybe             (fromJust)
import           Data.Time.Calendar     (Day, fromGregorian)
import           Eventful
import           Eventful.Store.Memory
import           Eventful.UUID          (uuidFromText)
import           Text.EmailAddress      (unsafeEmailAddress)

import           Player

marcoId :: PlayerId
marcoId = (PlayerId $ fromJust $ uuidFromText "c883a288-121f-4050-ac9c-13e1cfdd8490") -- unsafe!!

marco :: Player
marco = Player
    marcoId $
    PlayerData
        "Marco"
        "Perone"
        "Marcosh"
        (fromGregorian 1983 11 14)
        (unsafeEmailAddress "m.perone" "mvlabs.it") -- unsafe!!


andreaId :: PlayerId
andreaId = (PlayerId $ fromJust $ uuidFromText "223ff6a8-cb4e-475d-a7db-8077892e5877") -- unsafe!!

andrea :: Player
andrea = Player
    andreaId $
    PlayerData
        "Andrea"
        "Cadorin"
        "Cado"
        (fromGregorian 1989 1 25)
        (unsafeEmailAddress "a.cadorin" "mvlabs.it") -- unsafe!!

playersStoreExample :: IO ()
playersStoreExample = do
    tvar <- eventMapTVar
    let
        writer = tvarEventStoreWriter tvar
        reader = tvarEventStoreReader tvar
    let
        uuid = read "123e4567-e89b-12d3-a456-426655440000" -- id of the stream of events
        events =
            [ AddNewPlayer marco
            , AddNewPlayer andrea
            , RemovePlayer andreaId
            ]
    _ <- atomically $ storeEvents writer AnyVersion uuid events

    events' <- atomically $ getEvents reader (allEvents uuid)
    print $ latestProjection playersProjection $ fmap streamEventEvent events'
