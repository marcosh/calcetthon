{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Team where

import           Model.PlayerId
import           Score

-- aeson
import           Data.Aeson     (FromJSON, ToJSON, Value (Object), object,
                                 parseJSON, toJSON, withObject, (.:), (.=))

-- base
import           Data.Proxy     (Proxy (Proxy))
import           GHC.Generics   (Generic)

-- lens
import           Control.Lens   ((&), (.~), (?~))

-- swagger2
import           Data.Swagger   (NamedSchema (NamedSchema),
                                 SwaggerType (SwaggerObject), ToSchema,
                                 declareNamedSchema, declareSchemaRef,
                                 description, properties, type_)

data Team = Team
    { defence :: PlayerId
    , attack  :: PlayerId
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data TeamData = TeamData
    { team  :: Team
    , score :: Score
    }
    deriving (Eq, Show)

instance FromJSON TeamData where
    parseJSON = withObject "TeamData" $ \o -> TeamData
        <$> parseJSON (Object o)
        <*> o .: "score"

instance ToJSON TeamData where
    toJSON (TeamData dTeam dScore) = object
        [ "defence" .= defence dTeam
        , "attack"  .= attack dTeam
        , "score"   .= dScore
        ]

instance ToSchema TeamData where
    declareNamedSchema _ = do
        playerIdSchema <- declareSchemaRef (Proxy :: Proxy PlayerId)
        scoreSchema    <- declareSchemaRef (Proxy :: Proxy Score   )
        return $ NamedSchema (Just "TeamData") $ mempty
            & type_ .~ SwaggerObject
            & description ?~ "team data"
            & properties .~
                [ ("attack" , playerIdSchema)
                , ("defence", playerIdSchema)
                , ("score"  , scoreSchema   )
                ]
