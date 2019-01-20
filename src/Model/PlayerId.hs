{-# LANGUAGE OverloadedStrings #-}

module Model.PlayerId where

-- aeson
import           Data.Aeson             (FromJSON, ToJSON, parseJSON, toJSON,
                                         withText)

-- base
import           Data.Maybe             (maybe)

-- eventful-core
import           Eventful               (UUID, uuidFromText, uuidToText)

-- lens
import           Control.Lens           ((&), (.~), (?~))

-- persistent
import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql, sqlType)
import           Database.Persist.Types (PersistValue (..), SqlType (..))

-- swagger2
import           Data.Swagger           (NamedSchema (NamedSchema),
                                         SwaggerType (SwaggerString), ToSchema,
                                         declareNamedSchema, description,
                                         example, format, type_)

-- text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

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

instance PersistField PlayerId where
    toPersistValue playerId = PersistDbSpecific $ (encodeUtf8 . uuidToText . uuid_) playerId

    fromPersistValue (PersistDbSpecific uuidBs) = maybe
        (Left "impossible to parse as a uuid")
        Right
        (PlayerId <$> (uuidFromText . decodeUtf8) uuidBs)
    fromPersistValue _ = Left "unexpected field type. UUID required"


instance PersistFieldSql PlayerId where
    sqlType _ = SqlOther "UUID"
