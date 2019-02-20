{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.Model where

import           Model.Game
import           Model.Player

-- aeson
import           Data.Aeson   (FromJSON, ToJSON)

-- base
import           GHC.Generics (Generic)

-- eventful-base
import           Eventful     (Aggregate, Projection (..), serializedAggregate,
                               simpleSerializer)

data CalcetthonCommand
    = CalcetthonPlayerCommand PlayerCommand
    | CalcetthonGameCommand GameCommand

deserializeCalcetthonPlayerCommand :: CalcetthonCommand -> Maybe PlayerCommand
deserializeCalcetthonPlayerCommand (CalcetthonPlayerCommand command) = Just command
deserializeCalcetthonPlayerCommand _                                 = Nothing

deserializeCalcetthonGameCommand :: CalcetthonCommand -> Maybe GameCommand
deserializeCalcetthonGameCommand (CalcetthonGameCommand command) = Just command
deserializeCalcetthonGameCommand _                               = Nothing

data CalcetthonEvent
    = CalcetthonPlayerEvent PlayerEvent
    | CalcetthonGameEvent GameEvent
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

deserializeCalcetthonPlayerEvent :: CalcetthonEvent -> Maybe PlayerEvent
deserializeCalcetthonPlayerEvent (CalcetthonPlayerEvent event) = Just event
deserializeCalcetthonPlayerEvent _                             = Nothing

deserializeCalcetthonGameEvent :: CalcetthonEvent -> Maybe GameEvent
deserializeCalcetthonGameEvent (CalcetthonGameEvent event) = Just event
deserializeCalcetthonGameEvent _                           = Nothing

calcetthonPlayersAggregate :: Aggregate Players CalcetthonEvent CalcetthonCommand
calcetthonPlayersAggregate = serializedAggregate playersAggregate playersEventSerialized playersCommandSerializer
    where
        playersEventSerialized = simpleSerializer CalcetthonPlayerEvent deserializeCalcetthonPlayerEvent
        playersCommandSerializer = simpleSerializer CalcetthonPlayerCommand deserializeCalcetthonPlayerCommand

calcetthonGameAggregate :: Aggregate (Maybe Game) CalcetthonEvent CalcetthonCommand
calcetthonGameAggregate = serializedAggregate gameAggregate gameEventSerialized gameCommandSerializer
    where
        gameEventSerialized = simpleSerializer CalcetthonGameEvent deserializeCalcetthonGameEvent
        gameCommandSerializer = simpleSerializer CalcetthonGameCommand deserializeCalcetthonGameCommand

calcetthonPlayersProjection :: Projection Players CalcetthonEvent
calcetthonPlayersProjection = Projection (projectionSeed playersProjection) handleCalcetthonPlayersEvent
    where
        handleCalcetthonPlayersEvent :: Players -> CalcetthonEvent -> Players
        handleCalcetthonPlayersEvent players (CalcetthonPlayerEvent event) = (projectionEventHandler playersProjection) players event
        handleCalcetthonPlayersEvent players _ = players

calcetthonGameProjection :: Projection (Maybe Game) CalcetthonEvent
calcetthonGameProjection = Projection (projectionSeed gameProjection) handleCalcetthonGameEvent
    where
        handleCalcetthonGameEvent :: Maybe Game -> CalcetthonEvent -> Maybe Game
        handleCalcetthonGameEvent game (CalcetthonGameEvent event) = projectionEventHandler gameProjection game event
        handleCalcetthonGameEvent game _ = game

