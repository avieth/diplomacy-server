{-|
Module      : Resources.Start
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Resources.Start (

      resource

    ) where

import GHC.Generics
import Control.Monad.Trans.Class
import Control.Monad.State.Class as SC
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Stream as Stream
import Data.Aeson
import Data.JSON.Schema
import Data.Functor.Identity
import Data.Functor.Constant
import Data.List (sortBy, (\\))
import Data.Hourglass
import Rest
import Rest.Resource as R
import Diplomacy.GreatPower
import Diplomacy.Game
import Types.Server
import Types.GameId
import Types.GameState
import Types.UserData as UD
import Types.Credentials as Credentials

-- | Use this resource to start a game.
--
--     PUT : ask to start the game, passing the administrator credentials
--           in the request body.
--
resource :: Resource (ReaderT GameId Server) (ReaderT GameId Server) () Void Void
resource = mkResourceId
    { R.name = "start"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just update
    }
  where

    update :: Handler (ReaderT GameId Server)
    update = secureHandler $ mkInputHandler (jsonO . jsonE . jsonI) $ doUpdate

    doUpdate :: StartGameInput -> ExceptT (Reason StartGameError) (ReaderT GameId Server) StartGameOutput
    doUpdate input = withAdminCredentials creds startGame
      where

        StartGameInput creds = input

        startGame :: ExceptT (Reason StartGameError) (ReaderT GameId Server) StartGameOutput
        startGame = do
            state <- SC.get
            gameId <- lift ask
            let ds = randomDoubles state
            let t = currentTime state
            case M.lookup gameId (games state) of
                Nothing -> throwE NotFound
                Just (password, gameState) -> do
                    case startGameState t ds gameState of
                        Left e -> throwE (domainReason e)
                        Right gs -> do
                            SC.put (state { games = M.alter (const (Just (password, gs))) gameId (games state) })
                            return Started

-- | We assume the GameState has at most 7 registered users.
--   This is always true if the GameState is modified by registerUser only.
--   TODO should enforce this at the type level, but it's not so fragile so
--   whatever.
startGameState :: Elapsed -> Stream.Stream Double -> GameState -> Either StartGameError GameState
startGameState t randomDoubles game = case game of
    GameNotStarted map duration ->
        if notEnoughPlayers
        then Left NotEnoughPlayers
        else if tooManyPlayers
        then Left TooManyPlayers
        else Right (GameStarted map' (SomeGame newGame) Nothing duration t)
      where
        registered :: [(Username, Password)]
        registered = fmap (\(x, y) -> (x, UD.password y)) (M.toList map)
        registeredRandomOrder :: [(Username, Password)]
        registeredRandomOrder = fmap fst (sortBy (\(_, x) (_, y) -> x `compare` y) (Prelude.zip registered (toList randomDoubles)))
        controlUnits :: [S.Set GreatPower]
        controlUnits = case length registered of
            7 -> fmap S.singleton [minBound..maxBound]
            6 -> fmap S.singleton ([minBound..maxBound] \\ [Italy])
            5 -> fmap S.singleton ([minBound..maxBound] \\ [Germany, Italy])
            4 -> [S.singleton England, S.fromList [Austria, France], S.fromList [Germany, Turkey], S.fromList [Italy, Russia]]
            3 -> [S.fromList [England, Germany, Austria], S.fromList [Russia, Italy], S.fromList [France, Turkey]]
            _ -> []
        notEnoughPlayers = length registered < 3
        tooManyPlayers = length registered > 7
        -- We randomly assign each player to a control unit.
        assignments :: [((Username, Password), S.Set GreatPower)]
        assignments = Prelude.zip registeredRandomOrder controlUnits
        makeUserData :: ((Username, Password), S.Set GreatPower) -> (Username, UserData S.Set)
        makeUserData ((u, p), s) = (u, UserData p s)
        map' = M.fromList (fmap makeUserData assignments)
    GameStarted _ _ _ _ _ -> Left GameAlreadyStarted

newtype StartGameInput = StartGameInput Credentials

deriving instance Generic StartGameInput
deriving instance Typeable StartGameInput
instance FromJSON StartGameInput
instance ToJSON StartGameInput
instance JSONSchema StartGameInput where
    schema = gSchema

data StartGameOutput = Started

deriving instance Generic StartGameOutput
deriving instance Typeable StartGameOutput
instance FromJSON StartGameOutput
instance ToJSON StartGameOutput
instance JSONSchema StartGameOutput where
    schema = gSchema

data StartGameError = GameAlreadyStarted | NotEnoughPlayers | TooManyPlayers

deriving instance Generic StartGameError
deriving instance Typeable StartGameError
instance ToJSON StartGameError
instance JSONSchema StartGameError where
    schema = gSchema

instance ToResponseCode StartGameError where
    toResponseCode _ = 403
