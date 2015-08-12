{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Router (

      router

    ) where

import Control.Monad.Trans.Reader
import Rest
import Rest.Api
import Rest.Resource as R
import Resources.Game as Game
import Resources.Join as Join
import Resources.Start as Start
import Resources.Order as Order
import Resources.Advance as Advance
import Resources.Pause as Pause
import Resources.Metadata as Metadata
import Resources.Resolution as Resolution
import Resources.Client as Client
import Types.GameId
import Types.Server

router :: Router Server Server
router = root -/ client --/ game ---/ gameJoin
                                 ---/ gameStart
                                 ---/ gameOrder
                                 ---/ gameAdvance
                                 ---/ gamePause
                                 ---/ gameMetadata
                                 ---/ gameResolution
                        --/ admin

game :: Router (Server) (ReaderT GameId Server)
game = route Game.resource

gameJoin :: Router (ReaderT GameId Server) (ReaderT GameId Server)
gameJoin = route Join.resource

gameStart :: Router (ReaderT GameId Server) (ReaderT GameId Server)
gameStart = route Start.resource

gameOrder :: Router (ReaderT GameId Server) (ReaderT GameId Server)
gameOrder = route Order.resource

gameAdvance :: Router (ReaderT GameId Server) (ReaderT GameId Server)
gameAdvance = route Advance.resource

gamePause :: Router (ReaderT GameId Server) (ReaderT GameId Server)
gamePause = route Pause.resource

gameMetadata :: Router (ReaderT GameId Server) (ReaderT GameId Server)
gameMetadata = route Metadata.resource

gameResolution :: Router (ReaderT GameId Server) (ReaderT GameId Server)
gameResolution = route Resolution.resource

client :: Router Server Server
client = route Client.resource

admin :: Router (Server) (ReaderT GameId Server)
admin = route adminResource

adminResource :: Resource Server (ReaderT GameId Server) GameId Void Void
adminResource = mkResourceReader
    { R.name = "admin"
    , R.schema = noListing $ unnamedSingle GameId
    }
