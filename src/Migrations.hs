{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Migrations where

import           Model.GameId        (GameId)
import           Model.PlayerId      (PlayerId)

-- emailaddress
import           Text.EmailAddress   (EmailAddress)

-- persistent-template
import           Database.Persist.TH

-- time
import           Data.Time.Calendar  (Day)

share [mkPersist sqlSettings, mkMigrate "migrateReadModel"] [persistLowerCase|
Players
    uuid      PlayerId
    name      String
    surname   String
    nickname  String
    birthDate Day
    email     EmailAddress
    password  String
    Primary   uuid
    deriving Show

Games
    uuid          GameId
    redAttack  PlayerId
    redDefence PlayerId
    redScore   Int
    blueAttack   PlayerId
    blueDefence  PlayerId
    blueScore    Int
    Primary       uuid
    Foreign Players fkWinnerAttack  redAttack
    Foreign Players fkWinnerDefence redDefence
    Foreign Players fkLoserAttack   blueAttack
    Foreign Players fkLoserDefence  blueDefence
    deriving Show
|]
