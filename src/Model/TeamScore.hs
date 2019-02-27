{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.TeamScore where

import           Model.PlayerId
import           Model.Team
import           Score

-- aeson
import           Data.Aeson          (FromJSON, ToJSON, Value (Object),
                                      parseJSON, toJSON, withObject, (.:))

-- base
import           Data.Proxy          (Proxy (Proxy))

-- lens
import           Control.Lens        ((&), (.~), (?~))

-- swagger2
import           Data.Swagger        (NamedSchema (NamedSchema),
                                      SwaggerType (SwaggerObject), ToSchema,
                                      declareNamedSchema, declareSchemaRef,
                                      description, properties, type_)

-- text
import           Data.Text           (Text)

-- unordered-containers
import           Data.HashMap.Strict (insert)

data TeamScore player = TeamScore
    { team  :: Team player
    , score :: Score
    }
    deriving (Eq, Show)

instance FromJSON player => FromJSON (TeamScore player) where
    parseJSON = withObject "TeamScore" $ \o -> TeamScore
        <$> parseJSON (Object o)
        <*> o .: "score"

addObjectKey :: Text -> Value -> Value -> Value
addObjectKey key value (Object obj) = Object $ insert key value obj
addObjectKey _   _     whatever     = whatever

instance ToJSON player => ToJSON (TeamScore player) where
    toJSON (TeamScore dTeam dScore) = addObjectKey "score" (toJSON dScore) (toJSON dTeam)

instance ToSchema (TeamScore player) where
    declareNamedSchema _ = do
        playerSchema <- declareSchemaRef (Proxy :: Proxy PlayerId)
        scoreSchema  <- declareSchemaRef (Proxy :: Proxy Score )
        return $ NamedSchema (Just "TeamScore") $ mempty
            & type_ .~ SwaggerObject
            & description ?~ "team data"
            & properties .~
                [ ("attack" , playerSchema)
                , ("defence", playerSchema)
                , ("score"  , scoreSchema )
                ]
