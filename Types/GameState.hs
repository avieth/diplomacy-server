{-|
Module      : Types.GameState
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.GameState (

      SomeGame(..)
    , GameState(..)
    , GameStateView(..)
    , GameMetadata(..)
    , GameData(..)
    , GameResolution(..)

    , gameStateMatchCredentials
    , gameStateViewMetadata
    , gameStateViewData
    , gameStateViewResolution

    ) where

import GHC.Generics
import Control.DeepSeq
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.AtLeast
import Data.TypeNat.Nat
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Hourglass
import Data.Aeson hiding (One)
import Data.JSON.Schema as Schema
import Diplomacy.Game
import Diplomacy.Turn
import Diplomacy.Province
import Diplomacy.Zone
import Diplomacy.Aligned
import Diplomacy.Phase
import qualified Diplomacy.Order as DO
import qualified Diplomacy.OrderObject as DOO
import Diplomacy.OrderResolution
import qualified Diplomacy.Unit as DU
import qualified Diplomacy.GreatPower as DGP
import Diplomacy.Control
import Diplomacy.Occupation
import Types.Credentials as Cred
import Types.UserData as UD
import Types.Credentials (Username)
import Types.GreatPower
import Types.Unit
import Types.Order

data SomeGame where
    SomeGame :: Game round RoundUnresolved -> SomeGame

data GameState where

    GameNotStarted
        :: M.Map Username (UserData (Constant ()))
        -> Duration
        -> Duration
        -> GameState

    GameStarted
        :: M.Map Username (UserData S.Set)
        -- ^ we use Set rather than Identity, because one player can control
        --   more than one great power!
        -> AtLeast One SomeGame
        -- ^ All (unresolved) game states seen so far. First entry is the
        --   latest, and other entires retain the orders which were present
        --   when the game was advanced.
        -> Duration
        -- ^ Period of this game; after this duration, the game advances to the
        --   next round if it's in typical phase.
        -> Duration
        -- ^ Second period of this game; after this duration, the game advances
        --   to the next round if it's in adjust or retreat phase.
        -> Elapsed
        -- ^ Unix epoch time when this game was last advanced or when it was
        --   started.
        -> Bool
        -- ^ True iff the game is paused, meaning it should not advance
        --   automatically after a set amount of time.
        -> GameState

deriving instance Generic GameState
instance NFData GameState where rnf x = seq x ()

instance ToJSON GameState
instance FromJSON GameState

-- | The game state viewed as a certain player, who controls a set of
--   GreatPowers.
data GameStateView where
    GameNotStartedView :: GameStateView
    GameStartedView
        :: S.Set DGP.GreatPower
        -> AtLeast One SomeGame
        -> Duration
        -> Duration
        -> Elapsed
        -> Bool
        -> GameStateView

gameStateViewMetadata :: GameStateView -> Maybe GameMetadata
gameStateViewMetadata view = case view of
    GameNotStartedView -> Nothing
    GameStartedView greatPowers games duration secondDuration elapsed paused -> Just $ GameMetadata {
          metadataGreatPowers = S.map GreatPower greatPowers
        , metadataTurn = turn
        , metadataRound = round
        , metadataDuration = duration
        , metadataSecondDuration = secondDuration
        , metadataElapsed = elapsed
        , metadataPaused = paused
        }
      where
        game = Data.AtLeast.head games
        turn = case game of
            SomeGame game -> gameTurn game
        round = case game of
            SomeGame game -> gameRound game

-- | Number of places from the back of a list of SomeGame at which the game
--   for this turn and round is found.
gameIndex :: Turn -> Round -> Int
gameIndex turn round = roundToInt round + 5*(turnToInt turn)

findGame :: Turn -> Round -> AtLeast One SomeGame -> Maybe SomeGame
findGame turn round games =
    let gamesList = Data.AtLeast.toList games
        index = gameIndex turn round
    in  findGame' index (reverse gamesList)
  where
    findGame' :: Int -> [SomeGame] -> Maybe SomeGame
    findGame' n games = case games of
        [] -> Nothing
        (x : xs) -> if n == 0 then Just x else findGame' (n-1) xs

-- Careful! This will give the resolved orders for any turn/round pair found,
-- even if it's the latest (ongoing) game! The client-facing code must be
-- careful not to fulfill a user's request for the resolutions of the latest
-- round.
gameStateViewResolution :: Turn -> Round -> GameStateView -> Maybe GameResolution
gameStateViewResolution turn round view = case view of
    GameNotStartedView -> Nothing
    GameStartedView _ games _ _ _ _ -> case findGame turn round games of
        Nothing -> Nothing
        Just (SomeGame game) -> Just $ case resolve game of
            TypicalGame _ _ _ zonedResolvedOrders control -> GameResolutionTypical (M.map mapper zonedResolvedOrders) (M.map GreatPower control)
            RetreatGame _ _ _ _ zonedResolvedOrders occupation control -> GameResolutionRetreat (M.map mapper zonedResolvedOrders) (M.map (fmap Unit) occupation) (M.map GreatPower control)
            AdjustGame _ _ _ zonedResolvedOrders control -> GameResolutionAdjust (M.map mapper zonedResolvedOrders) (M.map GreatPower control)
  where
    mapper
        :: (Aligned DU.Unit, SomeResolved DOO.OrderObject phase)
        -> (Aligned Unit, SomeOrderObject phase, Bool)
    mapper (aunit, SomeResolved (object, res)) = (align unit greatPower, someObject, bool)
      where
        greatPower = alignedGreatPower aunit
        unit = Unit (alignedThing aunit)
        someObject = SomeOrderObject (DOO.SomeOrderObject object)
        bool = case res of
            Nothing -> True
            _ -> False

gameStateViewData :: Turn -> Round -> GameStateView -> Maybe GameData
gameStateViewData turn round view = case view of
    GameNotStartedView -> Nothing
    GameStartedView greatPowers games _ _ _ _ -> case findGame turn round games of
        Nothing -> Nothing
        Just (SomeGame game) -> Just $ case game of
            TypicalGame _ _ _ zonedOrders control -> GameDataTypical (M.map (picker greatPowers) zonedOrders) (M.map GreatPower control)
            RetreatGame _ _ _ _ zonedOrders occupation control -> GameDataRetreat (M.map (picker greatPowers) zonedOrders) (M.map (fmap Unit) occupation) (M.map GreatPower control)
            AdjustGame _ _ _ zonedOrders control -> GameDataAdjust (M.mapMaybe (pickerAdjust greatPowers) zonedOrders) (M.map GreatPower control)
  where
    -- | Picks the units and order objects to show: only objects for a great
    --   power in the parameter set are chosen.
    picker
        :: S.Set DGP.GreatPower
        -> (Aligned DU.Unit, DOO.SomeOrderObject phase)
        -> (Aligned Unit, Maybe (SomeOrderObject phase))
    picker greatPowers (aunit, someOrderObject) = (align unit greatPower, maybeSomeOrderObject)
      where
        greatPower = alignedGreatPower aunit
        unit = Unit (alignedThing aunit)
        maybeSomeOrderObject = case S.member greatPower greatPowers of
            True -> Just (SomeOrderObject someOrderObject)
            False -> Nothing
    -- | Picks the units and order objects to show, for adjust phase. This is
    --   different than the other two because we must take special care for
    --   build order objects! Not only should these *objects* be invisible to
    --   the other great powers, but so should their *subjects*!!!
    pickerAdjust
        :: S.Set DGP.GreatPower
        -> (Aligned DU.Unit, DOO.SomeOrderObject Adjust)
        -> Maybe (Aligned Unit, Maybe (SomeOrderObject Adjust))
    pickerAdjust greatPowers (aunit, someOrderObject) = case someOrderObject of
        DOO.SomeOrderObject (DOO.BuildObject) -> case S.member greatPower greatPowers of
            True -> Just (align unit greatPower, Just (SomeOrderObject someOrderObject))
            False -> Nothing
        DOO.SomeOrderObject _ -> Just $ case S.member greatPower greatPowers of
            True -> (align unit greatPower, Just (SomeOrderObject someOrderObject))
            False -> (align unit greatPower, Nothing)
      where
        greatPower = alignedGreatPower aunit
        unit = Unit (alignedThing aunit)

data GameMetadata = GameMetadata {
      metadataGreatPowers :: S.Set GreatPower
    , metadataTurn :: Turn
    , metadataRound :: Round
    , metadataDuration :: Duration
    , metadataSecondDuration :: Duration
    , metadataElapsed :: Elapsed
    , metadataPaused :: Bool
    }

instance ToJSON GameMetadata where
    toJSON metadata = object [
          "greatPowers" .= greatPowers
        , "turn" .= turn
        , "round" .= round
        , "duration" .= duration
        , "secondDuration" .= secondDuration
        , "elapsed" .= secondsElapsed
        , "paused" .= paused
        ]
      where
        greatPowers = metadataGreatPowers metadata
        turn = turnToInt (metadataTurn metadata)
        round = roundToInt (metadataRound metadata)
        -- We only show the duration up to the minute.
        -- These (hours and minutes) are Int64. Can't be bothered to
        -- import that so I can write the type signature.
        Hours hours = durationHours (metadataDuration metadata)
        Minutes minutes = durationMinutes (metadataDuration metadata)
        Hours secondHours = durationHours (metadataSecondDuration metadata)
        Minutes secondMinutes = durationMinutes (metadataSecondDuration metadata)
        -- seconds is an Int64 giving unix time when the game was last
        -- advanced (or started, in case it's round 1 turn 1).
        Elapsed (Seconds secondsElapsed) = metadataElapsed metadata
        duration = object [
              "hours" .= hours
            , "minutes" .= minutes
            ]
        secondDuration = object [
              "hours" .= secondHours
            , "minutes" .= secondMinutes
            ]
        paused = metadataPaused metadata

instance JSONSchema GameMetadata where
    schema _ = Schema.Object [
          greatPowersField
        , turnField
        , roundField
        , durationField
        , secondDurationField
        , elapsedField
        , pausedField
        ]
      where
        greatPowersField = Field "greatPowers" True (Schema.Array (LengthBound (Just 1) (Just 7)) True (Value unboundedLength))
        turnField = Field "turn" True (Schema.Number (Bound (Just 0) Nothing))
        roundField = Field "round" True (Schema.Number (Bound (Just 0) (Just 4)))
        durationField = Field "duration" True (Schema.Object [hoursField, minutesField])
        secondDurationField = Field "secondDuration" True (Schema.Object [hoursField, minutesField])
        elapsedField = Field "elapsed" True (Schema.Number (Bound (Just 0) Nothing))
        pausedField = Field "paused" True Boolean
        hoursField = Field "hours" True (Schema.Number (Bound (Just 0) Nothing))
        minutesField = Field "minutes" True (Schema.Number (Bound (Just 0) (Just 59)))

-- For now we give only a Bool to indicate success or failure (true for
-- success). In the future we should give the reason. This is not super easy
-- to do because we use GADTs with phase and order type parameters, so we
-- can't just derive generic JSON instances.
data GameResolution where
    GameResolutionTypical
        :: M.Map Zone (Aligned Unit, SomeOrderObject Typical, Bool)
        -> M.Map Province GreatPower
        -> GameResolution
    GameResolutionRetreat
        :: M.Map Zone (Aligned Unit, SomeOrderObject Retreat, Bool)
        -> M.Map Zone (Aligned Unit)
        -> M.Map Province GreatPower
        -> GameResolution
    GameResolutionAdjust
        :: M.Map Zone (Aligned Unit, SomeOrderObject Adjust, Bool)
        -> M.Map Province GreatPower
        -> GameResolution

instance ToJSON GameResolution where
    toJSON gameResolution = case gameResolution of
        GameResolutionTypical zonedResolvedOrders control -> object [
              "resolved" .= ZoneMap (fmap ResolvedTriple zonedResolvedOrders)
            , "control" .= ProvinceMap control
            ]
        GameResolutionRetreat zonedResolvedOrders occupation control -> object [
              "resolved" .= ZoneMap (fmap ResolvedTriple zonedResolvedOrders)
            , "occupation" .= ZoneMap occupation
            , "control" .= ProvinceMap control
            ]
        GameResolutionAdjust zonedResolvedOrders control -> object [
              "resolved" .= ZoneMap (fmap ResolvedTriple zonedResolvedOrders)
            , "control" .= ProvinceMap control
            ]

instance JSONSchema GameResolution where
    schema _ = Schema.Object []

data GameData where
    GameDataTypical
        :: M.Map Zone (Aligned Unit, Maybe (SomeOrderObject Typical))
        -> M.Map Province GreatPower
        -> GameData
    GameDataRetreat
        :: M.Map Zone (Aligned Unit, Maybe (SomeOrderObject Retreat))
        -> M.Map Zone (Aligned Unit)
        -> M.Map Province GreatPower
        -> GameData
    GameDataAdjust
        :: M.Map Zone (Aligned Unit, Maybe (SomeOrderObject Adjust))
        -> M.Map Province GreatPower
        -> GameData

instance ToJSON GameData where
    toJSON gameData = case gameData of
        GameDataTypical zonedOrders control -> object [
              "occupation" .= (ZoneMap (fmap UnresolvedPair zonedOrders))
            , "control" .= (ProvinceMap control)
            ]
        GameDataRetreat zonedOrders occupation control -> object [
              "dislodgement" .= (ZoneMap (fmap UnresolvedPair zonedOrders))
            , "occupation" .= (ZoneMap occupation)
            , "control" .= (ProvinceMap control)
            ]
        GameDataAdjust zonedOrders control -> object [
              "occupation" .= (ZoneMap (fmap UnresolvedPair zonedOrders))
            , "control" .= (ProvinceMap control)
            ]

instance JSONSchema GameData where
    schema _ = Schema.Object []

newtype ZoneMap t = ZoneMap (M.Map Zone t)

instance ToJSON t => ToJSON (ZoneMap t) where
    toJSON (ZoneMap map) = toJSON textKeyedMap
      where
        textKeyedMap :: M.Map T.Text t
        textKeyedMap = M.foldWithKey (\z x -> M.insert (zoneToText z) x) M.empty map
        zoneToText :: Zone -> T.Text
        zoneToText = printProvinceTarget . zoneProvinceTarget

newtype ProvinceMap t = ProvinceMap (M.Map Province t)

instance ToJSON t => ToJSON (ProvinceMap t) where
    toJSON (ProvinceMap map) = toJSON textKeyedMap
      where
        textKeyedMap :: M.Map T.Text t
        textKeyedMap = M.foldWithKey(\p x -> M.insert (printProvince p) x) M.empty map

instance ToJSON (Aligned Unit) where
    toJSON aunit = toJSON (greatPower, unit)
      where
        greatPower = GreatPower (alignedGreatPower aunit)
        unit = alignedThing aunit

newtype UnresolvedPair phase = UnresolvedPair (Aligned Unit, Maybe (SomeOrderObject phase))

instance ToJSON (UnresolvedPair phase) where
    toJSON (UnresolvedPair (aunit, maybeObject)) = toJSON (greatPower, unit, maybeObject)
      where
        greatPower = GreatPower (alignedGreatPower aunit)
        unit = alignedThing aunit

newtype ResolvedTriple phase = ResolvedTriple (Aligned Unit, SomeOrderObject phase, Bool)

instance ToJSON (ResolvedTriple phase) where
    toJSON (ResolvedTriple (aunit, object, bool)) = toJSON (greatPower, unit, object, bool)
      where
        greatPower = GreatPower (alignedGreatPower aunit)
        unit = alignedThing aunit

-- | Get a GameStateView for given Credentials, i.e. the set of GreatPowers
--   that this user controls, along with a SomeGame if the game is started,
--   no information if the game is not started, or Nothing in case the
--   Credentials do not match anyone.
gameStateMatchCredentials :: Credentials -> GameState -> Maybe GameStateView
gameStateMatchCredentials creds gameState = case gameState of
    GameNotStarted map _ _ -> case M.lookup uname map of
        Nothing -> Nothing
        Just ud -> case pwd == UD.password ud of
            False -> Nothing
            True -> Just GameNotStartedView
    GameStarted map someGames duration duration' elapsed paused -> case M.lookup uname map of
        Nothing -> Nothing
        Just ud -> case pwd == UD.password ud of
            False -> Nothing
            True -> Just (GameStartedView (UD.greatPower ud) someGames duration duration' elapsed paused)
  where
    uname = Cred.username creds
    pwd = Cred.password creds
