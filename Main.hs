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
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Reader
import qualified Data.Map as M
import Data.Monoid
import Data.Hourglass
import System.Hourglass
import Rest
import Rest.Resource as R
import Rest.Api
import Rest.Driver.Wai
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import Diplomacy.Game
import Types.GameState
import Types.ServerOptions as Options
import Types.Credentials
import Types.Server
import Types.GameId
import Resources.Game as Game
import Resources.Join as Join
import Resources.Start as Start
import Resources.Order as Order
import Resources.Advance as Advance
import Resources.Client as Client

main :: IO ()
main = do
    opts <- execParser (info Options.parser mempty)
    let username = adminUsername opts
    let password = adminPassword opts
    let cert = certificateFile opts
    let key = Options.keyFile opts
    let port = Options.port opts
    initialState <- serverState username password
    tvar <- newTVarIO initialState
    let app = waiApp tvar
    let daemon = advanceDaemon tvar
    async daemon
    --x <- mkJsApi (ModuleName "diplomacy") True (mkVersion 1 0 0) (router)
    --putStrLn x
    putStrLn ("Starting secure server on port " ++ show port)
    runTLS (tlsSettings cert key) (setPort port defaultSettings) app
    putStrLn "Goodbye"

waiApp :: TVar ServerState -> Application
waiApp tvar =
    let runner :: forall a . Server a -> IO a
        runner = runDiplomacyServer tvar
    in  apiToApplication runner api

-- To be run in a separate thread; this IO will periodically check every game
-- and advance it if it's started and hasn't been advanced for longer than its
-- period.
--
-- TODO this should get its own module. That would allow us to trim many
-- imports in this Main file.
advanceDaemon :: TVar ServerState -> IO ()
advanceDaemon tvar = do
    -- We check every half-minute.
    threadDelay (30 * oneSecond)
    advanceGamesIO tvar
    advanceDaemon tvar
  where
    -- threadDelay uses microseconds.
    oneSecond = 1000000
    advanceGamesIO :: TVar ServerState -> IO ()
    advanceGamesIO tvar = do
        t <- timeCurrent
        gameIds <- atomically $ do
            state <- readTVar tvar
            let (advancedGameIds, newState) = advanceState t state
            writeTVar tvar newState
            return advancedGameIds
        return ()
    advanceState :: Elapsed -> ServerState -> ([GameId], ServerState)
    advanceState t state = 
        let (gameIds, nextGames) = M.foldWithKey (advanceGameFold t) ([], M.empty) (games state)
        in  (gameIds, state { games = nextGames })
    advanceGameFold
        :: Elapsed
        -> GameId
        -> (Password, GameState)
        -> ([GameId], M.Map GameId (Password, GameState))
        -> ([GameId], M.Map GameId (Password, GameState))
    advanceGameFold t gameId (pwd, gameState) (ids, out) = case gameState of
        GameStarted m (SomeGame game) resolved duration duration' elapsed ->
            let Elapsed t' = t - elapsed 
                (observedDuration, _) = fromSeconds t'
                thresholdDuration = case game of
                    TypicalGame _ _ _ _ _ -> duration
                    RetreatGame _ _ _ _ _ _ _ -> duration'
                    AdjustGame _ _ _ _ _ -> duration'
            in  if observedDuration > thresholdDuration
                then let (someGame', someResolvedOrders) = Advance.advance (SomeGame game)
                     in  (gameId : ids, M.insert gameId (pwd, GameStarted m someGame' (Just someResolvedOrders) duration duration' t) out)
                else (ids, M.insert gameId (pwd, GameStarted m (SomeGame game) resolved duration duration' elapsed) out)
        _ -> (ids, M.insert gameId (pwd, gameState) out)

api = [(mkVersion 1 0 0, Some1 router)]

router :: Router Server Server
router = root -/ client --/ game ---/ gameJoin
                                 ---/ gameStart
                                 ---/ gameOrder
                                 ---/ gameAdvance
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

client :: Router Server Server
client = route Client.resource

admin :: Router (Server) (ReaderT GameId Server)
admin = route adminResource

-- | TODO in future versions, we may want to give the adminstrator some more
--   powers. If so, they'll in this resource.
adminResource :: Resource Server (ReaderT GameId Server) GameId Void Void
adminResource = mkResourceReader
    { R.name = "admin"
    , R.schema = noListing $ unnamedSingle GameId
    }
