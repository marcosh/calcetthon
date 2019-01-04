{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Migrations where

-- emailaddress
import           Text.EmailAddress    (EmailAddress)

-- persistent
import           Database.Persist.Sql

-- persistent-template
import           Database.Persist.TH

-- time
import           Data.Time.Calendar   (Day)

share [mkPersist sqlSettings, mkMigrate "migrateReadModel"] [persistLowerCase|
Players
    uuid String
    name String
    surname String
    nickname String
    birthDate Day
    email EmailAddress
|]
