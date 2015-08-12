{-|
Module      : Resources.Paused
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

module Resources.Pause (

      resource

    ) where

import GHC.Generics
import Data.Typeable
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.State.Class as SC
import qualified Data.Map as M
import Data.AtLeast
import Data.TypeNat.Vect
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
    { R.name = "pause"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just update
    }
  where

    update :: Handler (ReaderT GameId Server)
    update = secureHandler $ mkInputHandler (jsonO . jsonE . jsonI) $ doUpdate

    doUpdate :: PauseInput -> ExceptT (Reason PauseError) (ReaderT GameId Server) PauseOutput
    doUpdate input = withAdminCredentials creds pauseGame
      where
        PauseInput creds = input

    pauseGame :: ExceptT (Reason PauseError) (ReaderT GameId Server) PauseOutput
    pauseGame = do
        gameId <- lift ask
        modifyGameState gameId modifier
        return GamePaused

    modifier :: GameState -> ExceptT (Reason PauseError) (ReaderT GameId Server) GameState
    modifier gameState = case gameState of
        GameNotStarted _ _ _ -> throwE (domainReason PauseGameNotStarted)
        GameStarted m games duration duration' elapsed _ ->
            return (GameStarted m games duration duration' elapsed True)

data PauseError = PauseGameNotStarted

instance ToResponseCode PauseError where
    toResponseCode _ = 403

deriving instance Generic PauseError
deriving instance Typeable PauseError
instance FromJSON PauseError
instance ToJSON PauseError
instance JSONSchema PauseError where
    schema = gSchema

newtype PauseInput = PauseInput Credentials

deriving instance Generic PauseInput
deriving instance Typeable PauseInput
instance FromJSON PauseInput
instance ToJSON PauseInput
instance JSONSchema PauseInput where
    schema = gSchema

data PauseOutput = GamePaused

deriving instance Generic PauseOutput
deriving instance Typeable PauseOutput
instance FromJSON PauseOutput
instance ToJSON PauseOutput
instance JSONSchema PauseOutput where
    schema = gSchema
