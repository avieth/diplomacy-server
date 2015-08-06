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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Types.Server (

      ServerState(..)
    , Server

    , withAdminCredentials
    , withUserCredentialsForGame
    , modifyGameState

    , serverState
    , runDiplomacyServer

    ) where

import Control.DeepSeq
import Control.Monad.Trans.Except
import Control.Monad.Trans.State as S
import Control.Monad.State.Class as SC
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Stream as Stream
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Functor.Identity
import Data.Hourglass
import System.Hourglass
import System.Random
import Rest.Error
import Types.GameId
import Types.Credentials
import Types.GameState

data ServerState = ServerState {
      adminCredentials :: Credentials
    , games :: M.Map GameId (Password, GameState)
    , currentTime :: Elapsed
    , randomDoubles :: Stream.Stream Double
    , clientHtml :: BL.ByteString
    }

type Server = StateT ServerState Identity

withAdminCredentials
    :: MonadState ServerState m
    => Credentials
    -> ExceptT (Reason e) m t
    -> ExceptT (Reason e) m t
withAdminCredentials creds term = do
    state <- SC.get
    if adminCredentials state == creds
    then term
    else throwE AuthenticationFailed

withUserCredentialsForGame
    :: MonadState ServerState m
    => Credentials
    -> GameId
    -> (GameStateView -> ExceptT (Reason e) m t)
    -> ExceptT (Reason e) m t
withUserCredentialsForGame creds gameId term = do
    state <- SC.get
    let game = M.lookup gameId (games state)
    case game of
        Nothing -> throwE NotFound
        Just (_, gameState) -> case gameStateMatchCredentials creds gameState of
            Nothing -> throwE AuthenticationFailed
            Just gameStateView -> term gameStateView

modifyGameState
    :: MonadState ServerState m
    => GameId
    -> (GameState -> ExceptT (Reason e) m GameState)
    -> ExceptT (Reason e) m ()
modifyGameState gameId f = do
    state <- SC.get
    let game = M.lookup gameId (games state)
    case game of
        Nothing -> throwE NotFound
        Just (password, gameState) -> do
            modified <- f gameState
            SC.put (state { games = M.alter (const (Just (password, force modified))) gameId (games state) })
            return ()

serverState :: Username -> Password -> IO ServerState
serverState adminUsername adminPassword = do
    let games = M.empty
    g <- getStdGen
    let ds = unfold random g :: Stream.Stream Double
    t <- timeCurrent
    bs <- BL.readFile "client.html"
    return $ ServerState (Credentials adminUsername adminPassword) games t ds bs

runDiplomacyServer :: TVar ServerState -> (forall a . Server a -> IO a)
runDiplomacyServer tvar m = do
    -- Always refurnish the state with random doubles.
    g <- getStdGen
    let ds = unfold random g :: Stream.Stream Double
    t <- timeCurrent
    bs <- BL.readFile "client.html"
    atomically $ do
        state <- readTVar tvar
        -- It's important to update the time and random doubles before we
        -- run the term @m@.
        let state' = state { currentTime = t, randomDoubles = ds, clientHtml = bs }
        let (x, nextState) = runIdentity (runStateT m state')
        writeTVar tvar nextState
        return x
