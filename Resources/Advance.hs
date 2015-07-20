{-|
Module      : Resources.Advance
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Resources.Advance (

      resource

    ) where

import GHC.Generics
import Data.Typeable
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.State.Class as SC
import Data.Aeson
import Data.JSON.Schema
import Rest
import Rest.Resource as R
import Diplomacy.Game
import Types.Server
import Types.GameId
import Types.Credentials
import Types.GameState

resource :: Resource (ReaderT GameId Server) (ReaderT GameId Server) () Void Void
resource = mkResourceId
    { R.name = "advance"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just update
    }
  where

    update :: Handler (ReaderT GameId Server)
    update = secureHandler $ mkInputHandler (jsonO . jsonE . jsonI) $ doUpdate

    doUpdate :: AdvanceInput -> ExceptT (Reason AdvanceError) (ReaderT GameId Server) AdvanceOutput
    doUpdate input = withAdminCredentials creds advanceGame
      where
        creds = credentials input

    advanceGame :: ExceptT (Reason AdvanceError) (ReaderT GameId Server) AdvanceOutput
    advanceGame = do
        gameId <- lift ask
        modifyGameState gameId modifier
        return GameAdvanced

    modifier :: GameState -> ExceptT (Reason AdvanceError) (ReaderT GameId Server) GameState
    modifier gameState = case gameState of
        GameNotStarted _ _ -> throwE (domainReason AdvanceGameNotStarted)
        GameStarted m someGame duration elapsed -> do
            state <- SC.get
            return (GameStarted m (nextGame someGame) duration (currentTime state))

    nextGame :: SomeGame -> SomeGame
    nextGame (SomeGame game) = case game of
        TypicalGame _ Unresolved _ _ _ -> SomeGame $ resolve game
        RetreatGame _ Unresolved _ _ _ _ _ -> SomeGame $ resolve game
        AdjustGame _ Unresolved _ _ _ -> SomeGame $ resolve game
        TypicalGame _ Resolved _ _ _ -> SomeGame $ continue game
        RetreatGame _ Resolved _ _ _ _ _ -> SomeGame $ continue game
        AdjustGame _ Resolved _ _ _ -> SomeGame $ continue game

data AdvanceError = AdvanceGameNotStarted

instance ToResponseCode AdvanceError where
    toResponseCode _ = 403

deriving instance Generic AdvanceError
deriving instance Typeable AdvanceError
instance FromJSON AdvanceError
instance ToJSON AdvanceError
instance JSONSchema AdvanceError where
    schema = gSchema

data AdvanceInput = AdvanceInput {
      credentials :: Credentials
    }

deriving instance Generic AdvanceInput
deriving instance Typeable AdvanceInput
instance FromJSON AdvanceInput
instance ToJSON AdvanceInput
instance JSONSchema AdvanceInput where
    schema = gSchema

data AdvanceOutput = GameAdvanced

deriving instance Generic AdvanceOutput
deriving instance Typeable AdvanceOutput
instance FromJSON AdvanceOutput
instance ToJSON AdvanceOutput
instance JSONSchema AdvanceOutput where
    schema = gSchema
