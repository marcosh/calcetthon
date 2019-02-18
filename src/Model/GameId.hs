{-# LANGUAGE OverloadedStrings #-}

module Model.GameId where

-- aeson
import           Data.Aeson             (FromJSON, ToJSON, parseJSON, toJSON,
                                         withText)

-- base
import           Text.Read              (Read, readPrec)

-- eventful-core
import           Eventful               (UUID, uuidFromText, uuidToText)

-- http-api-data
import           Web.HttpApiData        (FromHttpApiData, ToHttpApiData,
                                         parseUrlPiece, toUrlPiece)

-- lens
import           Control.Lens           ((&), (.~), (?~))

-- path-pieces
import           Web.PathPieces         (PathPiece, fromPathPiece, toPathPiece)

-- persistent
import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql, sqlType)
import           Database.Persist.Types (PersistValue (..), SqlType (..))

-- swagger2
import           Data.Swagger           (NamedSchema (..),
                                         SwaggerType (SwaggerString), ToSchema,
                                         declareNamedSchema, description,
                                         example, format, type_)

-- text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

newtype GameId = GameId { uuid_ :: UUID }
    deriving (Eq, Ord, Show)

instance Read GameId where
    readPrec = GameId <$> readPrec

instance FromJSON GameId where
    parseJSON = withText "GameId" $ \gameId
        -> maybe (fail "invalid UUID") (pure . GameId) (uuidFromText gameId)

instance ToJSON GameId where
    toJSON (GameId uuid) = toJSON uuid

instance ToSchema GameId where
    declareNamedSchema _ = return $ NamedSchema (Just "gameId") $ mempty
        & type_ .~ SwaggerString
        & description ?~ "gameId"
        & format ?~ "uuid"
        & example ?~ "a2e2ef2a-ca1b-4038-8767-b196ea4516af"

instance PersistField GameId where
    toPersistValue gameId = PersistDbSpecific $ (encodeUtf8 . uuidToText . uuid_) gameId

    fromPersistValue (PersistDbSpecific uuidBs) = maybe
        (Left "impossible to parse as a uuid")
        Right
        (GameId <$> (uuidFromText . decodeUtf8) uuidBs)
    fromPersistValue _ = Left "unexpected field type. UUID required"

instance PersistFieldSql GameId where
    sqlType _ = SqlOther "UUID"

instance PathPiece GameId where
    fromPathPiece t = GameId <$> uuidFromText t
    toPathPiece     = uuidToText . uuid_

instance ToHttpApiData GameId where
    toUrlPiece = uuidToText . uuid_

instance FromHttpApiData GameId where
    parseUrlPiece gameId = maybe (fail "invalid UUID") (pure . GameId) (uuidFromText gameId)
