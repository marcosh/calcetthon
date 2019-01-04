{-# LANGUAGE OverloadedStrings #-}

module Model.Email where

-- aeson
import           Data.Aeson        (FromJSON, ToJSON, parseJSON, toJSON,
                                    withText)

-- emailaddress
import           Text.EmailAddress (EmailAddress, validateFromText)

-- lens
import           Control.Lens      ((&), (.~), (?~))

-- swagger2
import           Data.Swagger      (NamedSchema (NamedSchema),
                                    SwaggerType (SwaggerString), ToSchema,
                                    declareNamedSchema, description, example,
                                    format, type_)

newtype Email = Email { emailAddress_ :: EmailAddress }
    deriving (Eq, Show)

instance FromJSON Email where
    parseJSON = withText "Email" $ \e -> Email <$>
        either fail pure (validateFromText e)

instance ToJSON Email where
    toJSON (Email emailAddr) = toJSON emailAddr

instance ToSchema Email where
    declareNamedSchema _ = return $ NamedSchema (Just "email") $ mempty
        & type_ .~ SwaggerString
        & description ?~ "email"
        & format ?~ "email"
        & example ?~ "gigi@zucon.com"
