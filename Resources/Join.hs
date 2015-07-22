{-|
Module      : Resources.Join
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

module Resources.Join (

      resource

    ) where

import GHC.Generics
import Control.Monad.State.Class as SC
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Typeable
import qualified Data.Map as M
import Data.Functor.Constant as FC
import Data.Aeson
import Data.JSON.Schema
import Rest
import Rest.Resource as R
import Types.GameId
import Types.UserData hiding (password)
import Types.Credentials hiding (password)
import qualified Types.Credentials as Credentials
import Types.GameState
import Types.Server

-- | Use this resource to join a game.
--
--     PUT : ask to join the game, passing the game password and your
--           credentials in the request body.
--
resource :: Resource (ReaderT GameId Server) (ReaderT GameId Server) () Void Void
resource = mkResourceId
    { R.name = "join"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just update
    }
  where
    update :: Handler (ReaderT GameId Server)
    update = secureHandler $ mkInputHandler (jsonO . jsonE . jsonI) $ doUpdate
    doUpdate :: JoinGameInput -> ExceptT (Reason JoinGameError) (ReaderT GameId Server) JoinGameOutput
    doUpdate input = do
        gameId <- lift ask
        state <- SC.get
        case M.lookup gameId (games state) of
            Nothing -> throwE NotFound
            Just (pwd', gameState) -> case pwd' == pwd of
                False -> throwE NotAllowed
                True -> case registerUser creds gameState of
                    Left e -> throwE (domainReason e)
                    Right nextGameState -> do
                        SC.put (state { games = M.alter (const (Just (pwd', nextGameState))) gameId (games state) })
                        return Joined
      where
        creds = credentials input
        pwd = password input

registerUser :: Credentials -> GameState -> Either JoinGameError GameState
registerUser credentials gameState = case gameState of
    GameNotStarted map duration -> case M.lookup uname map of
        Nothing -> 
            if M.size map == 7
            then Left GameFull
            else Right (GameNotStarted (M.insert uname userData map) duration)
          where
            userData = UserData pwd (FC.Constant ())
        Just _ -> Left UsernameTaken
    GameStarted _ _ _ _ _ -> Left GameAlreadyStarted
  where
    uname = Credentials.username credentials
    pwd = Credentials.password credentials

data JoinGameInput = JoinGameInput {
      password :: Password
      -- ^ Password for the game.
    , credentials :: Credentials
      -- ^ Credentials of the user who wishes to join.
    }

deriving instance Generic JoinGameInput
deriving instance Typeable JoinGameInput
instance FromJSON JoinGameInput
instance ToJSON JoinGameInput
instance JSONSchema JoinGameInput where
    schema = gSchema

data JoinGameOutput = Joined

deriving instance Generic JoinGameOutput
deriving instance Typeable JoinGameOutput
instance FromJSON JoinGameOutput
instance ToJSON JoinGameOutput
instance JSONSchema JoinGameOutput where
    schema = gSchema

data JoinGameError
    = UsernameTaken
    | GameAlreadyStarted
    | GameFull
    deriving (Show, Generic, Typeable)

instance JSONSchema JoinGameError where
    schema = gSchema

instance ToJSON JoinGameError

instance ToResponseCode JoinGameError where
    toResponseCode UsernameTaken = 403
    toResponseCode GameAlreadyStarted = 403
    toResponseCode GameFull = 403
