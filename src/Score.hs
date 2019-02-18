{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Score where

import           Control.Lens                 ((&), (?~))
import           Control.Monad                ((>=>))
import           Data.Aeson                   (FromJSON (parseJSON),
                                               ToJSON (toJSON), Value (Number))
import           Data.Aeson.Types             (typeMismatch)
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Scientific              (toBoundedInteger)
import           Data.Swagger
import           Data.Swagger.Internal.Schema (toNamedSchema)
import           GHC.Generics                 (Generic)

data Score
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    deriving (Eq, Show, Generic)

instance Ord Score where
    Zero  <= _    = True
    One   <= Zero = False
    One   <= _    = True
    Two   <= Zero = False
    Two   <= One  = False
    Two   <= _    = True
    Three <= Zero = False
    Three <= One  = False
    Three <= Two  = False
    Three <= _    = True
    Four  <= Four = True
    Four  <= Five = True
    Four  <= Six  = True
    Four  <= _    = False
    Five  <= Five = True
    Five  <= Six  = True
    Five  <= _    = False
    Six   <= Six  = True
    Six   <= _    = False

intToScore :: Int -> Maybe Score
intToScore 0 = Just Zero
intToScore 1 = Just One
intToScore 2 = Just Two
intToScore 3 = Just Three
intToScore 4 = Just Four
intToScore 5 = Just Five
intToScore 6 = Just Six
intToScore _ = Nothing

scoreToInt :: Score -> Int
scoreToInt Zero  = 0
scoreToInt One   = 1
scoreToInt Two   = 2
scoreToInt Three = 3
scoreToInt Four  = 4
scoreToInt Five  = 5
scoreToInt Six   = 6

instance FromJSON Score where
    parseJSON (Number i) =
        case (toBoundedInteger >=> intToScore) i of
            Just score -> pure score
            Nothing    -> typeMismatch "Score" (Number i)
    parseJSON invalid = typeMismatch "Score" invalid

instance ToJSON Score where
    toJSON = toJSON . scoreToInt

instance ToSchema Score where
    declareNamedSchema _ = return $ toNamedSchema (Proxy :: Proxy Int)
        & maximum_ ?~ 6
        & minimum_ ?~ 0
