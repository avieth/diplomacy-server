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

module Types.GameState (

      SomeGame(..)
    , SomeResolvedOrders(..)
    , GameState(..)
    , GameStateView(..)

    , gameStateMatchCredentials

    ) where

import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Hourglass
import Data.Aeson
import Data.JSON.Schema as Schema
import Diplomacy.Game
import Diplomacy.Turn
import Diplomacy.Province
import Diplomacy.Zone
import Diplomacy.Aligned
import Diplomacy.Phase
import Diplomacy.OrderObject
import Diplomacy.OrderResolution
import qualified Diplomacy.Unit as DU
import qualified Diplomacy.GreatPower as DGP
import Types.Credentials as Cred
import Types.UserData as UD
import Types.Credentials (Username)
import Types.GreatPower
import Types.Unit
import Types.Order

data SomeGame where
    SomeGame :: Game round RoundUnresolved -> SomeGame

data SomeResolvedOrders where
    SomeResolvedOrders
        :: M.Map Zone (Aligned DU.Unit, SomeResolved OrderObject phase)
        -> SomeResolvedOrders

data GameState where

    GameNotStarted
        :: M.Map Username (UserData (Constant ()))
        -> Duration
        -> GameState

    GameStarted
        :: M.Map Username (UserData S.Set)
        -- ^ we use Set rather than Identity, because one player can control
        --   more than one great power!
        -> SomeGame
        -> Maybe SomeResolvedOrders
        -> Duration
        -- ^ Period of this game; after this duration, the game advances to the
        --   next stage (resolve then continue).
        -> Elapsed
        -- ^ Unix epoch time when this game was last advanced or when it was
        --   started.
        -> GameState

{-
instance Show GameState where
    show gameState = case gameState of
        GameNotStarted map _ -> "Game not started"
        GameStarted map (SomeGame game) _ _ -> case game of
            TypicalGame _ Unresolved _ _ _ -> showGame game
            RetreatGame _ Unresolved _ _ _ _ _ -> showGame game
            AdjustGame _ Unresolved _ _ _ -> showGame game
-}

-- | The game state viewed as a certain player, who controls a set of
--   GreatPowers.
data GameStateView where
    GameNotStartedView :: GameStateView
    GameStartedView
        :: S.Set DGP.GreatPower
        -> SomeGame
        -> Maybe SomeResolvedOrders
        -> Duration
        -> Elapsed
        -> GameStateView

-- | This show instance is useful for debugging but should not be exposed
--   to clients, as it shows all orders of all great powers!
instance Show GameStateView where
    show gameStateView = case gameStateView of
        GameNotStartedView -> "Game not started"
        GameStartedView greatPowers (SomeGame game) someResolved duration elapsed ->
            concat [
              show greatPowers
            , "\n"
            , showGame game
            ]

-- | Dumping a GameStateView to JSON...
--
--   {
--     tag : 'GameNotStartedView' | 'GameStartedView'
--   , components : {} | {
--       metadata : <metadata>
--     , greatPowers : [GreatPower]
--     , occupation : <occupation/orders>
--     , dislodgement : <dislodgement/orders>
--     , control : <control>
--     ]
--   }
instance ToJSON GameStateView where
    toJSON gameStateView = case gameStateView of
        GameNotStartedView -> object ["tag" .= ("GameNotStarted" :: T.Text)]
        GameStartedView greatPowers (SomeGame game) maybeResolved duration elapsed -> object [
              "tag" .= ("GameStarted" :: T.Text)
            , "components" .= object [
                "metadata" .= object [
                  "turn" .= turn
                , "round" .= round
                , "durationHours" .= hours
                , "durationMinutes" .= minutes
                , "elapsed" .= seconds
                ]
              -- We throw our GreatPower newtype over the
              -- Diplomacy.GreatPower.GreatPower types so that we can ToJSON
              -- this set.
              , "greatPowers" .= S.map GreatPower greatPowers
              , "occupation" .= occupation
              , "dislodgement" .= dislodgement
              , "control" .= control
              , "resolved" .= resolved
              ]
            ]
          where

            -- Turn is 0-indexed.
            turn = turnToInt (gameTurn game)
            -- Round is 0-indexed.
            round = roundToInt (gameRound game)
            -- We only show the duration up to the minute.
            -- These (hours and minutes) are Int64. Can't be bothered to
            -- import that so I can write the type signature.
            Hours hours = durationHours duration
            Minutes minutes = durationMinutes duration
            -- seconds is an Int64 giving unix time when the game was last
            -- advanced (or started, in case it's round 1 turn 1).
            Elapsed (Seconds seconds) = elapsed

            -- The third component is the order object, but due to my own
            -- personal lethargy I have decided to dump it to T.Text via
            -- printObject (from Types.Order) instead of giving it a wrapper
            -- and a ToJSON instance.
            occupation :: M.Map T.Text (GreatPower, Unit, Maybe T.Text)
            occupation = case game of
                TypicalGame TypicalRoundOne Unresolved _ _ _ ->
                    M.foldWithKey typicalOccupationFold M.empty (gameZonedOrders game)
                TypicalGame TypicalRoundTwo Unresolved _ _ _ ->
                    M.foldWithKey typicalOccupationFold M.empty (gameZonedOrders game)
                RetreatGame _ _ _ _ _ _ _ ->
                    M.foldWithKey occupationFold M.empty (gameOccupation game)
                AdjustGame _ _ _ _ _ ->
                    M.foldWithKey occupationFold M.empty (gameOccupation game)

            typicalOccupationFold
                :: Zone
                -> (Aligned DU.Unit, SomeOrderObject Typical)
                -> M.Map T.Text (GreatPower, Unit, Maybe T.Text)
                -> M.Map T.Text (GreatPower, Unit, Maybe T.Text)
            typicalOccupationFold zone (aunit, SomeOrderObject object) = M.insert key (greatPower, unit, orderObjectText)
              where
                unit = Unit (alignedThing aunit)
                dgpGreatPower = alignedGreatPower aunit
                greatPower = GreatPower dgpGreatPower
                key = printProvinceTarget (zoneProvinceTarget zone)
                subject = (alignedThing aunit, zoneProvinceTarget zone)
                orderObjectText :: Maybe T.Text
                orderObjectText = do
                    guard (dgpGreatPower `S.member` greatPowers)
                    return $ printObject subject object

            occupationFold
                :: Zone
                -> Aligned DU.Unit
                -> M.Map T.Text (GreatPower, Unit, Maybe T.Text)
                -> M.Map T.Text (GreatPower, Unit, Maybe T.Text)
            occupationFold zone aunit = M.insert key (greatPower, unit, Nothing)
              where
                key = printProvinceTarget (zoneProvinceTarget zone)
                unit = Unit (alignedThing aunit)
                greatPower = GreatPower (alignedGreatPower aunit)

            dislodgement :: Maybe (M.Map T.Text (GreatPower, Unit, Maybe T.Text, Maybe Bool))
            dislodgement = case game of
                RetreatGame RetreatRoundOne Unresolved t _ _ _ _ ->
                    Just $ M.foldWithKey (dislodgementFold Unresolved) M.empty (gameZonedOrders game)
                RetreatGame RetreatRoundTwo Unresolved t _ _ _ _ ->
                    Just $ M.foldWithKey (dislodgementFold Unresolved) M.empty (gameZonedOrders game)
                _ -> Nothing

            dislodgementFold
                :: Status roundStatus
                -> Zone
                -> (Aligned DU.Unit, RoundOrderConstructor roundStatus Retreat)
                -> M.Map T.Text (GreatPower, Unit, Maybe T.Text, Maybe Bool)
                -> M.Map T.Text (GreatPower, Unit, Maybe T.Text, Maybe Bool)
            dislodgementFold status zone (aunit, x) = M.insert key (greatPower, unit, orderObjectText, resolutionBool)
              where
                unit = Unit (alignedThing aunit)
                dgpGreatPower = alignedGreatPower aunit
                greatPower = GreatPower dgpGreatPower
                key = printProvinceTarget (zoneProvinceTarget zone)
                subject = (alignedThing aunit, zoneProvinceTarget zone)
                orderObjectText :: Maybe T.Text
                orderObjectText = case (status, x) of
                    (Unresolved, SomeOrderObject object) -> do
                        guard (dgpGreatPower `S.member` greatPowers)
                        return $ printObject subject object
                    (Resolved, SomeResolved (object, _)) -> Just $ printObject subject object
                resolutionBool = case (status, x) of
                    (Unresolved, _) -> Nothing
                    (Resolved, SomeResolved (_, resolution)) -> Just $ maybe True (const False) resolution


            control :: M.Map T.Text GreatPower
            control = M.foldWithKey controlFold M.empty (gameControl game)
            controlFold :: Province -> DGP.GreatPower -> M.Map T.Text GreatPower -> M.Map T.Text GreatPower
            -- Here we are careful to wrap the DGP.GreatPower in our
            -- JSON-able newtype of the same name.
            controlFold p g = M.insert (printProvince p) (GreatPower g)

            resolved :: M.Map T.Text (GreatPower, Unit, Bool)
            resolved = case maybeResolved of
                Just (SomeResolvedOrders map) ->
                    M.foldWithKey resolvedFold M.empty map
                _ -> M.empty

            resolvedFold
                :: Zone
                -> (Aligned DU.Unit, SomeResolved OrderObject phase)
                -> M.Map T.Text (GreatPower, Unit, Bool)
                -> M.Map T.Text (GreatPower, Unit, Bool)
            resolvedFold zone (aunit, SomeResolved (object, resolution)) =
                M.insert key (greatPower, unit, succeeded)
              where
                key = printProvinceTarget (zoneProvinceTarget zone)
                greatPower = GreatPower (alignedGreatPower aunit)
                unit = Unit (alignedThing aunit)
                succeeded = maybe True (const False) resolution

instance FromJSON GameStateView where
    parseJSON v = fail "FromJSON GameStateView not implemented"

instance JSONSchema GameStateView where
    schema _ = Choice [notStartedSchema, startedSchema]
      where
        notStartedSchema = Schema.Object [
              Field "tag" True (Value (LengthBound (Just l) (Just l)))
            ]
          where
            l = length ("GameNotStarted" :: String)
        startedSchema = Schema.Object [
              Field "tag" True (Value (LengthBound (Just l) (Just l)))
            , Field "components" True components
            ]
          where
            l = length ("GameStarted" :: String)
            components = Schema.Object [
                  Field "metadata" True metadata
                , Field "greatPowers" True greatPowers
                , Field "occupation" True occupation
                , Field "dislodgement" False dislodgement
                , Field "control" True control
                , Field "resolved" False resolved
                ]
            metadata = Schema.Object [
                  Field "turn" True (Schema.Number (Bound (Just 0) Nothing))
                , Field "round" True (Schema.Number (Bound (Just 0) (Just 4)))
                , Field "durationHours" True (Schema.Number (Bound (Just 0) Nothing))
                , Field "durationMinutes" True (Schema.Number (Bound (Just 0) Nothing))
                , Field "elapsed" True (Schema.Number (Bound (Just 0) Nothing))
                ]
            greatPowers = Schema.Array (LengthBound (Just 1) (Just 3)) True (Value (LengthBound Nothing Nothing))
            -- TODO can't be bothered to write these up... what a pain in the
            -- ass. We'll just leave them as Any.
            occupation = Schema.Any
            dislodgement = Schema.Any
            control = Schema.Any
            resolved = Schema.Any

-- | Get a GameStateView for given Credentials, i.e. the set of GreatPowers
--   that this user controls, along with a SomeGame if the game is started,
--   no information if the game is not started, or Nothing in case the
--   Credentials do not match anyone.
gameStateMatchCredentials :: Credentials -> GameState -> Maybe GameStateView
gameStateMatchCredentials creds gameState = case gameState of
    GameNotStarted map _ -> case M.lookup uname map of
        Nothing -> Nothing
        Just ud -> case pwd == UD.password ud of
            False -> Nothing
            True -> Just GameNotStartedView
    GameStarted map someGame someResolved duration elapsed -> case M.lookup uname map of
        Nothing -> Nothing
        Just ud -> case pwd == UD.password ud of
            False -> Nothing
            True -> Just (GameStartedView (UD.greatPower ud) someGame someResolved duration elapsed)
  where
    uname = Cred.username creds
    pwd = Cred.password creds
