{-|
Module      : Resources.Game.Remove
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
{-# LANGUAGE FlexibleContexts #-}

module Resources.Game.Remove (

      RemoveGameInput(..)
    , RemoveGameOutput(..)
    , removeGame

    ) where

import GHC.Generics
import Control.Monad.State.Class as SC
import Control.Monad.Trans.Except
import Data.Typeable
import qualified Data.Map as M
import Data.Aeson
import Data.JSON.Schema
import Rest
import Types.GameId
import Types.Server
import Types.Credentials

data RemoveGameInput = RemoveGameInput {
      gameId :: GameId
    , credentials :: Credentials
    }

deriving instance Generic RemoveGameInput
deriving instance Typeable RemoveGameInput
instance FromJSON RemoveGameInput
instance ToJSON RemoveGameInput
instance JSONSchema RemoveGameInput where
    schema = gSchema

data RemoveGameOutput where
    GameRemoved :: RemoveGameOutput

deriving instance Generic RemoveGameOutput
deriving instance Typeable RemoveGameOutput
instance FromJSON RemoveGameOutput
instance ToJSON RemoveGameOutput
instance JSONSchema RemoveGameOutput where
    schema = gSchema

removeGame
    :: MonadState ServerState m
    => GameId
    -> ExceptT (Reason Void) m RemoveGameOutput
removeGame gameId = do
    state <- SC.get
    case M.member gameId (games state) of
        False -> throwE NotFound
        True -> do
            let nextState = state { games = M.delete gameId (games state) }
            SC.put nextState
            return GameRemoved
