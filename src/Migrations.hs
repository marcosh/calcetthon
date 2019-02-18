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
    winnerAttack  PlayerId
    winnerDefence PlayerId
    winnerScore   Int
    loserAttack   PlayerId
    loserDefence  PlayerId
    loserScore    Int
    Primary       uuid
    Foreign Players fkWinnerAttack  winnerAttack
    Foreign Players fkWinnerDefence winnerDefence
    Foreign Players fkLoserAttack   loserAttack
    Foreign Players fkLoserDefence  loserDefence
    deriving Show
|]
