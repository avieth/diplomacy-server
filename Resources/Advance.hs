{-|
Module      : Resources.Advance
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
{-# LANGUAGE GADTs #-}

module Resources.Advance (

      resource
    , advance

    ) where

import GHC.Generics
import Data.Typeable
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.State.Class as SC
import qualified Data.Map as M
import Data.Aeson
import Data.JSON.Schema
import Rest
import Rest.Resource as R
import Diplomacy.Game
import Types.Server
import Types.GameId
import Types.Credentials
import Types.GameState

resource :: Resource (ReaderT GameId Server) (ReaderT GameId Server) () Void Void
resource = mkResourceId
    { R.name = "advance"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just update
    }
  where

    update :: Handler (ReaderT GameId Server)
    update = secureHandler $ mkInputHandler (jsonO . jsonE . jsonI) $ doUpdate

    doUpdate :: AdvanceInput -> ExceptT (Reason AdvanceError) (ReaderT GameId Server) AdvanceOutput
    doUpdate input = withAdminCredentials creds advanceGame
      where
        AdvanceInput creds = input

    advanceGame :: ExceptT (Reason AdvanceError) (ReaderT GameId Server) AdvanceOutput
    advanceGame = do
        gameId <- lift ask
        modifyGameState gameId modifier
        return GameAdvanced

    modifier :: GameState -> ExceptT (Reason AdvanceError) (ReaderT GameId Server) GameState
    modifier gameState = case gameState of
        GameNotStarted _ _ _ -> throwE (domainReason AdvanceGameNotStarted)
        GameStarted m someGame someResolved duration duration' elapsed -> do
            state <- SC.get
            let (nextGame, resolved) = advance someGame
            return (GameStarted m nextGame (Just resolved) duration duration' (currentTime state))

-- | Advance a game, resolving and then continuing, so that we also go from
--   Unresolved to Unresolved. Retreat and Adjust phases in which there is
--   nothing to do are skipped.
advance :: SomeGame -> (SomeGame, SomeResolvedOrders)
advance (SomeGame game) = case game of
    TypicalGame TypicalRoundOne Unresolved _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
            resolvedOrders = gameZonedResolvedOrders resolved
        in  case M.size (gameDislodged continued) of
                -- Automatically skip retreat phases where nobody is
                -- dislodged.
                0 -> advance (SomeGame continued)
                _ -> (SomeGame continued, SomeResolvedOrders resolvedOrders)
    TypicalGame TypicalRoundTwo Unresolved _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
            resolvedOrders = gameZonedResolvedOrders resolved
        in  case M.size (gameDislodged continued) of
                -- Automatically skip retreat phases where nobody is
                -- dislodged.
                0 -> advance (SomeGame continued)
                _ -> (SomeGame continued, SomeResolvedOrders resolvedOrders)
    RetreatGame RetreatRoundOne Unresolved _ _ _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
            resolvedOrders = gameZonedResolvedOrders resolved
        in  (SomeGame continued, SomeResolvedOrders resolvedOrders)
    RetreatGame RetreatRoundTwo Unresolved _ _ _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
            resolvedOrders = gameZonedResolvedOrders resolved
            defecits = fmap ((flip gameSupplyCentreDefecit) continued) [minBound..maxBound]
        in  if all (== 0) defecits
            -- Automatically skip adjust phases where nobody has a defecit,
            -- positive or negative.
            then advance (SomeGame continued)
            else (SomeGame continued, SomeResolvedOrders resolvedOrders)
    AdjustGame _ Unresolved _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
            resolvedOrders = gameZonedResolvedOrders resolved
        in  (SomeGame continued, SomeResolvedOrders resolvedOrders)

data AdvanceError = AdvanceGameNotStarted

instance ToResponseCode AdvanceError where
    toResponseCode _ = 403

deriving instance Generic AdvanceError
deriving instance Typeable AdvanceError
instance FromJSON AdvanceError
instance ToJSON AdvanceError
instance JSONSchema AdvanceError where
    schema = gSchema

newtype AdvanceInput = AdvanceInput Credentials

deriving instance Generic AdvanceInput
deriving instance Typeable AdvanceInput
instance FromJSON AdvanceInput
instance ToJSON AdvanceInput
instance JSONSchema AdvanceInput where
    schema = gSchema

data AdvanceOutput = GameAdvanced

deriving instance Generic AdvanceOutput
deriving instance Typeable AdvanceOutput
instance FromJSON AdvanceOutput
instance ToJSON AdvanceOutput
instance JSONSchema AdvanceOutput where
    schema = gSchema
