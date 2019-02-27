{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.Team where

-- base
import           GHC.Generics (Generic)

-- aeson
import           Data.Aeson   (FromJSON, ToJSON)

data Team player = Team
    { defence :: player
    , attack  :: player
    }
    deriving (Eq, Show, Functor, Generic, ToJSON, FromJSON)
