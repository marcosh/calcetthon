{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.PlayerData where

import           Model.Email

-- aeson
import           Data.Aeson            (FromJSON, parseJSON, withObject, (.:))

-- base
import           Data.Proxy            (Proxy (Proxy))

-- bcrypt
import           Crypto.BCrypt         (fastBcryptHashingPolicy,
                                        hashPasswordUsingPolicy)

-- bytestring
import           Data.ByteString.Char8 (pack, unpack)

-- lens
import           Control.Lens          ((&), (.~), (?~))

-- swagger2
import           Data.Swagger          (NamedSchema (NamedSchema),
                                        SwaggerType (SwaggerObject), ToSchema,
                                        declareNamedSchema, declareSchemaRef,
                                        description, properties, type_)

-- time
import           Data.Time.Calendar    (Day)

data PlayerData = PlayerData
    { name      :: String
    , surname   :: String
    , nickname  :: String
    , birthDate :: Day
    , email     :: Email
    , password  :: String
    }
    deriving (Eq, Show)

instance FromJSON PlayerData where
    parseJSON = withObject "Player" $ \p -> PlayerData
        <$> p .: "name"
        <*> p .: "surname"
        <*> p .: "nickname"
        <*> p .: "birthDate"
        <*> p .: "email"
        <*> p .: "password"

instance ToSchema PlayerData where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy :: Proxy String)
        daySchema    <- declareSchemaRef (Proxy :: Proxy Day   )
        emailSchema  <- declareSchemaRef (Proxy :: Proxy Email )
        return $ NamedSchema (Just "playerData") $ mempty
            & type_ .~ SwaggerObject
            & description ?~ "data of a player"
            & properties .~
                [ ("name",      stringSchema)
                , ("surname",   stringSchema)
                , ("nickname",  stringSchema)
                , ("birthDate", daySchema   )
                , ("email"    , emailSchema )
                , ("password" , stringSchema)
                ]

hashPassword :: PlayerData -> IO (Maybe PlayerData)
hashPassword (PlayerData name' surname' nickname' birthDate' email' password') = do
    hashedPassword <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password')
    pure $ PlayerData name' surname' nickname' birthDate' email' . unpack <$> hashedPassword
