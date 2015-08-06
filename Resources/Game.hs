{-|
Module      : Resources.Game
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

module Resources.Game (

      resource

    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State as S
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import qualified Data.Map as M
import Data.Hourglass
import Rest
import Rest.Resource as R
import Rest.Dictionary.Types
import Types.Server
import Types.GameId
import Types.Credentials
import Types.GameState
import Resources.Game.Create as Create
import Resources.Game.Remove as Remove
import Diplomacy.Game
import Diplomacy.Turn

-- | The game resource will use a string identifier to pick out a game, and
--   give methods
--     GET : all info about the game state
--     GET : listing of the games
--     CREATE : make a new game
--     DELETE : destroy a game
--   TBD parameterize on the great power who's asking?!
--   Admin | Player GreatPower
--   Admin | Player String? Use player names rather than their power?
resource :: Resource Server (ReaderT GameId Server) GameId () Void
resource = mkResourceReader
    { R.name = "game"
    , R.schema = withListing () $ unnamedSingle GameId
    --, R.schema = withListing () $ named [("id", singleBy GameId)]
    -- We use update (PUT) where we really ought to use get (GET) because
    -- there is a lot of resistance against including bodies in GET requests,
    -- so much so that the W3C XMLHttpRequest NEVER includes a body in a
    -- GET request.
    , R.update = Just get
    -- TODO TBD listing doesn't work; unsupported route. Why?!?
    , R.list = const listing
    , R.create = Just create
    , R.remove = Just remove
    }
  where

    get :: Handler (ReaderT GameId Server)
    get = secureHandler $ mkHandler (jsonO . jsonE . jsonI) handler

    handler :: Env h p Credentials -> ExceptT (Reason Void) (ReaderT GameId Server) (Maybe GameData)
    handler env =
        let credentials = input env
        in  lift ask >>= \gameId -> doGet credentials gameId

    doGet
        :: Credentials
        -> GameId
        -> ExceptT (Reason Void) (ReaderT GameId Server) (Maybe GameData)
    doGet credentials gameId = withUserCredentialsForGame credentials gameId f
      where
        f gameStateView = return $ do
            metadata <- gameStateViewMetadata gameStateView
            let turn = metadataTurn metadata
            let round = metadataRound metadata
            gameStateViewData turn round gameStateView

    listing :: ListHandler Server
    listing = mkListing (stringO) $ \_ -> lift doListing
    doListing :: Server [String]
    doListing = listGames >>= return . fmap show

    create :: Handler Server
    create = secureHandler $ mkInputHandler (jsonO . jsonE . jsonI) $ doCreate
    doCreate :: CreateGameInput -> ExceptT (Reason Void) Server CreateGameOutput
    doCreate input = withAdminCredentials creds (lift (createGame gameId password duration duration'))
      where
        creds = Create.credentials input
        gameId = Create.gameId input
        password = Create.gamePassword input
        duration = maybe (makeDuration 15) makeDuration (Create.gameDuration input)
        duration' = maybe (makeDuration 5) makeDuration (Create.gameSecondDuration input)
        makeDuration x = Duration (fromIntegral 0) (fromIntegral x) (fromIntegral 0) (fromIntegral 0)

    remove :: Handler (ReaderT GameId Server)
    remove = secureHandler $ mkIdHandler (jsonO . jsonE . jsonI) $ \credentials gameId -> doRemove (RemoveGameInput gameId credentials)
    doRemove :: RemoveGameInput -> ExceptT (Reason Void) (ReaderT GameId Server) RemoveGameOutput
    doRemove input = withAdminCredentials creds (removeGame gameId)
      where
        creds = Remove.credentials input
        gameId = Remove.gameId input

listGames :: Server [GameId]
listGames = do
    state <- S.get
    return $ M.keys (games state)
