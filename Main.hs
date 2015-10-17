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
{-# LANGUAGE GADTs #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.AtLeast
import Data.TypeNat.Vect
import Data.Monoid
import Data.Hourglass
import System.Hourglass
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
import Router
import qualified Resources.Advance as Advance
import System.Console.Haskeline hiding (defaultSettings)
import qualified System.Console.Haskeline as Haskeline

main :: IO ()
main = do
    opts <- execParser (info Options.parser mempty)
    password <- runInputT Haskeline.defaultSettings getThePassword
    let username = adminUsername opts
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

  where

    getThePassword = do
        x <- getPassword (Just '*') "Password:"
        case x of
            Nothing -> getThePassword
            Just x' -> return x'

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
    -- We check every second.
    threadDelay oneSecond
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
        GameStarted m (AtLeast (VCons (SomeGame game) VNil) rest) duration duration' elapsed paused ->
            let Elapsed t' = t - elapsed 
                (observedDuration, _) = fromSeconds t'
                thresholdDuration = case game of
                    TypicalGame _ _ _ _ _ -> duration
                    RetreatGame _ _ _ _ _ _ _ -> duration'
                    AdjustGame _ _ _ _ _ -> duration'
            in  if not paused && observedDuration > thresholdDuration
                then let nextGame = Advance.advance (SomeGame game)
                     in  (gameId : ids, M.insert gameId (pwd, GameStarted m (AtLeast (VCons nextGame VNil) ((SomeGame game) : rest)) duration duration' t False) out)
                else (ids, M.insert gameId (pwd, GameStarted m (AtLeast (VCons (SomeGame game) VNil) rest) duration duration' elapsed paused) out)
        _ -> (ids, M.insert gameId (pwd, gameState) out)

api = [(mkVersion 1 0 0, Some1 router)]


