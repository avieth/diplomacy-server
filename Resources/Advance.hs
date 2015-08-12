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
import Data.AtLeast
import Data.TypeNat.Vect
import Data.Aeson
import Data.JSON.Schema
import Rest
import Rest.Resource as R
import Diplomacy.Game
import Diplomacy.SupplyCentreDeficit
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
        GameStarted m (AtLeast (VCons someGame VNil) rest) duration duration' elapsed _ -> do
            state <- SC.get
            let nextGame = advance someGame
            return (GameStarted m (AtLeast (VCons nextGame VNil) (someGame : rest)) duration duration' (currentTime state) False)

-- | Advance a game, resolving and then continuing, so that we also go from
--   Unresolved to Unresolved. Retreat and Adjust phases in which there is
--   nothing to do are skipped, but they are not forgotten! They're given
--   in the return value.
advance :: SomeGame -> SomeGame
advance (SomeGame game) = SomeGame (continue (resolve game))
{-
advance (SomeGame game) = case game of
    TypicalGame TypicalRoundOne Unresolved _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
        in  case M.size (gameDislodged continued) of
                -- Automatically skip retreat phases where nobody is
                -- dislodged.
                0 -> advance (SomeGame continued)
                _ -> AtLeast (VCons (SomeGame continued) VNil) []
    TypicalGame TypicalRoundTwo Unresolved _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
        in  case M.size (gameDislodged continued) of
                -- Automatically skip retreat phases where nobody is
                -- dislodged.
                0 -> advance (SomeGame continued)
                _ -> SomeGame continued
    RetreatGame RetreatRoundOne Unresolved _ _ _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
        in  SomeGame continued
    RetreatGame RetreatRoundTwo Unresolved _ _ _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
            occupation = gameOccupation continued
            control = gameControl continued
            deficits = fmap (\greatPower -> supplyCentreDeficit greatPower occupation control) [minBound..maxBound]
        in  if all (== 0) deficits
            -- Automatically skip adjust phases where nobody has a deficit,
            -- positive or negative.
            -- TODO must also check that, in case there's a negative
            -- deficit, this great power actually has a place to build.
            -- Really, we ought to use the order synthesizers to see whether
            -- any player actually has a choice. That goes for typical
            -- phase as well; it's rare, but it could happen, maybe?
            -- No, that would cause infinite loop behaviour in advance.
            then advance (SomeGame continued)
            else SomeGame continued
    AdjustGame _ Unresolved _ _ _ ->
        let resolved = resolve game
            continued = continue resolved
        in  SomeGame continued
-}

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
