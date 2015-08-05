{-|
Module      : Resources.Metadata
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Resources.Metadata (

      resource

    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Rest
import Rest.Resource as R
import Types.Server
import Types.GameState
import Types.GameId
import Types.Credentials

resource :: Resource (ReaderT GameId Server) (ReaderT GameId Server) () Void Void
resource = mkResourceId
    { R.name = "metadata"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just get
    }
  where
    get :: Handler (ReaderT GameId Server)
    get = secureHandler $ mkHandler (jsonO . jsonE . jsonI) handler

    handler :: Env h p Credentials -> ExceptT (Reason Void) (ReaderT GameId Server) (Maybe GameMetadata)
    handler env =
        let credentials = input env
        in  lift ask >>= \gameId -> doGet credentials gameId

    doGet
        :: Credentials
        -> GameId
        -> ExceptT (Reason Void) (ReaderT GameId Server) (Maybe GameMetadata)
    doGet credentials gameId = withUserCredentialsForGame credentials gameId (return . gameStateViewMetadata)
