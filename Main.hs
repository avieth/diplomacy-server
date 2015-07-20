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
{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Reader
import Data.Monoid
import Rest
import Rest.Resource as R
import Rest.Api
import Rest.Driver.Wai
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import Types.ServerOptions as Options
import Types.Credentials
import Types.Server
import Types.GameId
import Resources.Game as Game
import Resources.Join as Join
import Resources.Start as Start
import Resources.Order as Order
import Resources.Advance as Advance

main :: IO ()
main = do
    opts <- execParser (info Options.parser mempty)
    let username = adminUsername opts
    let password = adminPassword opts
    let cert = certificateFile opts
    let key = Options.keyFile opts
    let port = Options.port opts
    app <- waiApp username password
    --x <- mkJsApi (ModuleName "diplomacy") True (mkVersion 1 0 0) (router)
    --putStrLn x
    putStrLn ("Starting secure server on port " ++ show port)
    runTLS (tlsSettings cert key) (setPort port defaultSettings) app
    putStrLn "Goodbye"

waiApp :: Username -> Password -> IO Application
waiApp username password = do
    initialState <- serverState username password
    tvar <- newTVarIO initialState
    let runner :: forall a . Server a -> IO a
        runner = runDiplomacyServer tvar
    return $ apiToApplication runner api

api = [(mkVersion 1 0 0, Some1 router)]

router :: Router Server Server
router = root -/ game --/ gameJoin
                      --/ gameStart
                      --/ gameOrder
                      --/ gameAdvance
              -/ admin

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

admin :: Router (Server) (ReaderT GameId Server)
admin = route adminResource

-- | TODO in future versions, we may want to give the adminstrator some more
--   powers. If so, they'll in this resource.
adminResource :: Resource Server (ReaderT GameId Server) GameId Void Void
adminResource = mkResourceReader
    { R.name = "admin"
    , R.schema = noListing $ unnamedSingle GameId
    }
