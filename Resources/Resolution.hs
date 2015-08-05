{-|
Module      : Resources.Resolution
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Resources.Resolution (

      resource

    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Rest
import Rest.Resource as R
import Rest.Dictionary.Types
import Types.Server
import Types.GameState
import Types.GameId
import Types.Credentials
import Diplomacy.Game
import Diplomacy.Turn

resource :: Resource (ReaderT GameId Server) (ReaderT GameId Server) () Void Void
resource = mkResourceId
    { R.name = "resolution"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just get
    }
  where
    get :: Handler (ReaderT GameId Server)
    get = secureHandler $ mkHandler (addPar roundParam . mkPar turnParam . jsonO . jsonE . jsonI) handler

    turnParam :: Param (Maybe Turn)
    turnParam = Param ["turn"] $ \xs -> case xs of
        (Just x : _) -> case reads x :: [(Int, String)] of
            [(i, [])] -> case turnFromInt i of
                Just t -> Right (Just t)
                _ -> Left (ParseError "Could not parse turn")
            _ -> Left (ParseError "Could not parse turn")
        [Nothing] -> Right Nothing
        _ -> Left (ParseError "Could not parse turn")

    roundParam :: Param (Maybe Round)
    roundParam = Param ["round"] $ \xs -> case xs of
        (Just x : _) -> case reads x :: [(Int, String)] of
            [(i, [])] -> let min = minBound :: Round
                             max = maxBound :: Round
                         in  if i >= fromEnum min && i <= fromEnum max
                             then Right (Just (toEnum i))
                             else Left (ParseError "Could not parse round")
            _ -> Left (ParseError "Could not parse round")
        [Nothing] -> Right Nothing
        _ -> Left (ParseError "Could not parse round")


    handler :: Env h (Maybe Round, Maybe Turn) Credentials -> ExceptT (Reason Void) (ReaderT GameId Server) (Maybe GameResolution)
    handler env =
        let credentials = input env
            (maybeRound, maybeTurn) = param env
        in  lift ask >>= \gameId -> doGet credentials gameId maybeTurn maybeRound

    doGet
        :: Credentials
        -> GameId
        -> Maybe Turn
        -> Maybe Round
        -> ExceptT (Reason Void) (ReaderT GameId Server) (Maybe GameResolution)
    doGet credentials gameId maybeTurn maybeRound = withUserCredentialsForGame credentials gameId f
      where
        f gameStateView = return $ do
            metadata <- gameStateViewMetadata gameStateView
            let turn' = maybe (metadataTurn metadata) id maybeTurn
            -- Prevent giving the resolution of the latest (currently ongoing)
            -- round! This would allow any player to see the orders of other
            -- players.
            let (turn, round) = case maybeRound of
                    Just x -> (turn', x)
                    Nothing -> if metadataRound metadata /= RoundOne
                               then (turn', prevRound (metadataRound metadata))
                               else case prevTurn turn' of
                                   -- In this case we're at turn 0 round 0; we
                                   -- give the current turn and round and the
                                   -- upcoming guard will fail.
                                   Nothing -> (turn', metadataRound metadata)
                                   Just t -> (t, prevRound (metadataRound metadata))
            guard (turn <= metadataTurn metadata)
            guard (turn < metadataTurn metadata || round < metadataRound metadata)
            gameStateViewResolution turn round gameStateView
