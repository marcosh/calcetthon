{-# LANGUAGE OverloadedStrings #-}

module Model.PlayerId where

-- aeson
import           Data.Aeson   (FromJSON, ToJSON, parseJSON, toJSON, withText)

-- eventful-core
import           Eventful     (UUID, uuidFromText)

-- lens
import           Control.Lens ((&), (.~), (?~))

-- swagger2
import           Data.Swagger (NamedSchema (NamedSchema),
                               SwaggerType (SwaggerString), ToSchema,
                               declareNamedSchema, description, example, format,
                               type_)

newtype PlayerId = PlayerId { uuid_ :: UUID }
    deriving (Eq, Show)

instance FromJSON PlayerId where
    parseJSON = withText "PlayerId" $ \playerId
        -> maybe (fail "invalid UUID") (pure . PlayerId) (uuidFromText playerId)

instance ToJSON PlayerId where
    toJSON (PlayerId uuid) = toJSON uuid

instance ToSchema PlayerId where
    declareNamedSchema _ = return $ NamedSchema (Just "playerId") $ mempty
        & type_ .~ SwaggerString
        & description ?~ "playerId"
        & format ?~ "uuid"
        & example ?~ "a2e2ef2a-ca1b-4038-8767-b196ea4516af"
