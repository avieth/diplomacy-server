{-|
Module      : Resources.Game.Create
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Resources.Game.Create (

      CreateGameInput(..)
    , CreateGameOutput(..)
    , createGame

    ) where

import GHC.Generics
import Control.Monad.Trans.State as S
import Data.Typeable
import qualified Data.Map as M
import Data.Aeson
import Data.JSON.Schema
import Data.Hourglass
import Types.GameId
import Types.Server
import Types.Credentials
import Types.GameState

data CreateGameInput = CreateGameInput {
      gameId :: GameId
    , gamePassword :: Password
    -- ^ A password for the game, to share with the people who you wish to
    -- allow to join.
    , credentials :: Credentials
    -- ^ Administrator credentials
    , gameDuration :: Maybe Int
    }

deriving instance Generic CreateGameInput
deriving instance Typeable CreateGameInput
instance FromJSON CreateGameInput
instance ToJSON CreateGameInput
instance JSONSchema CreateGameInput where
    schema = gSchema

data CreateGameOutput where
    GameCreated :: CreateGameOutput
    NameAlreadyTaken :: CreateGameOutput

deriving instance Generic CreateGameOutput
deriving instance Typeable CreateGameOutput
instance FromJSON CreateGameOutput
instance ToJSON CreateGameOutput
instance JSONSchema CreateGameOutput where
    schema = gSchema

createGame :: GameId -> Password -> Duration -> Server CreateGameOutput
createGame gameId password duration = do
    state <- S.get
    case M.member gameId (games state) of
        True -> return $ NameAlreadyTaken
        False -> do
            let gameState = GameNotStarted M.empty duration
            let nextState = state { games = M.insert gameId (password, gameState) (games state) }
            S.put nextState
            return GameCreated
