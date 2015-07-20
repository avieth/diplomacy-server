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

module Types.GameState (

      SomeGame(..)
    , GameState(..)
    , GameStateView(..)

    , gameStateMatchCredentials

    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Hourglass
import Data.Aeson
import Data.JSON.Schema
import Diplomacy.Game
import Diplomacy.GreatPower
import Types.Credentials as Cred
import Types.UserData as UD
import Types.Credentials (Username)

data SomeGame where
    SomeGame :: Game round roundStatus -> SomeGame

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
        -> Duration
        -- ^ Period of this game; after this duration, the game advances to the
        --   next stage (resolve then continue).
        -> Elapsed
        -- ^ Unix epoch time when this game was last advanced or when it was
        --   started.
        -> GameState

instance Show GameState where
    show gameState = case gameState of
        GameNotStarted map _ -> "GameNotStarted"
        GameStarted map (SomeGame game) _ _ -> case game of
            TypicalGame _ Unresolved _ _ _ -> showGame game
            RetreatGame _ Unresolved _ _ _ _ _ -> showGame game
            AdjustGame _ Unresolved _ _ _ -> showGame game

-- | The game state viewed as a certain GreatPower.
data GameStateView where
    GameNotStartedView :: GameStateView
    GameStartedView :: S.Set GreatPower -> SomeGame -> GameStateView

instance Show GameStateView where
    show gameStateView = case gameStateView of
        GameNotStartedView -> "Game not started"
        GameStartedView greatPowers (SomeGame game) -> concat [
              show greatPowers
            , "\n"
            , showGame game
            ]

instance JSONSchema GameStateView where
    schema _ = Choice [notStartedSchema, startedSchema]
      where
        notStartedSchema = Value (LengthBound (Just l) (Just l))
          where
            l = length ("GameNotStarted" :: String)
        -- For a started game we want
        --
        --   metadata
        --     turn [1900, Infinity)
        --     round [1, 5]
        --
        --   control of provinces
        --     map Province GreatPower
        --   in the JSON we'll just leave out uncontrolled provines.
        --
        --   occupation/dislodgement with orders for THIS great power.
        --   
        startedSchema = undefined

instance ToJSON GameStateView where
    toJSON gameStateView = case gameStateView of
        GameNotStartedView -> String "GameNotStarted"
        GameStartedView greatPowers someGame -> String "GameStarted"

gameStateMatchCredentials :: Credentials -> GameState -> Maybe GameStateView
gameStateMatchCredentials creds gameState = case gameState of
    GameNotStarted map _ -> case M.lookup uname map of
        Nothing -> Nothing
        Just ud -> case pwd == UD.password ud of
            False -> Nothing
            True -> Just GameNotStartedView
    GameStarted map gameState _ _ -> case M.lookup uname map of
        Nothing -> Nothing
        Just ud -> case pwd == UD.password ud of
            False -> Nothing
            True -> Just (GameStartedView (UD.greatPower ud) gameState)
  where
    uname = Cred.username creds
    pwd = Cred.password creds
