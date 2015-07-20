{-|
Module      : Resources.Order
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
{-# LANGUAGE DataKinds #-}

module Resources.Order (

      resource

    ) where

import GHC.Generics
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Typeable
import Data.Aeson
import Data.JSON.Schema
import qualified Data.Set as S
import Rest
import Rest.Resource as R
import qualified Diplomacy.Phase as Phase
import Diplomacy.Game
import Diplomacy.Aligned
import qualified Diplomacy.GreatPower as DGP
import Types.Order
import Types.Credentials
import Types.GreatPower
import Types.GameId
import Types.Server
import Types.GameState

-- | The order resource will give methods
--     PUT : ask to issue an order
resource :: Resource (ReaderT GameId Server) (ReaderT GameId Server) () Void Void
resource = mkResourceId
    { R.name = "order"
    , R.schema = singleton () $ unnamedSingle (const ())
    , R.update = Just update
    }
  where

    update :: Handler (ReaderT GameId Server)
    update = secureHandler $ mkInputHandler (jsonO . jsonE . jsonI) $ doUpdate

    doUpdate :: IssueOrdersInput -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) IssueOrdersOutput
    doUpdate input = do
        gameId <- lift ask
        withUserCredentialsForGame creds gameId (issueOrders gameId)
      where

        creds = credentials input
        issuedOrders = orders input

        issueOrders
            :: GameId
            -> GameStateView
            -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) IssueOrdersOutput
        issueOrders gameId gameStateView = do
            x <- issueOrders' gameStateView
            modifyGameState gameId (return . modifier x)
            return OrdersIssued
        
        modifier :: SomeGame -> GameState -> GameState
        modifier (SomeGame x) state = case state of
            GameNotStarted y duration -> GameNotStarted y duration -- Impossible; should not have this case...
            GameStarted m _ duration elapsed -> GameStarted m (SomeGame x) duration elapsed

        issueOrders' :: GameStateView -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) SomeGame
        issueOrders' gameStateView = case gameStateView of
            GameNotStartedView -> throwE (domainReason IssueOrderGameNotStarted)
            GameStartedView greatPower (SomeGame someGame) -> case (someGame, issuedOrders) of
                (TypicalGame TypicalRoundOne Unresolved x y z, Typical os) -> issueOrders'' greatPower os someGame
                (RetreatGame RetreatRoundOne Unresolved _ _ _ _ _, Retreat os) -> issueOrders'' greatPower os someGame
                (TypicalGame TypicalRoundTwo Unresolved x y z, Typical os) -> issueOrders'' greatPower os someGame
                (RetreatGame RetreatRoundTwo Unresolved _ _ _ _ _, Retreat os) -> issueOrders'' greatPower os someGame
                (AdjustGame AdjustRound Unresolved _ _ _, Adjust os) -> issueOrders'' greatPower os someGame
                _ -> throwE (domainReason IssueOrderInvalid)

        issueOrders''
            :: S.Set DGP.GreatPower
            -> [(GreatPower, Order (RoundPhase round))]
            -> Game round RoundUnresolved
            -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) SomeGame
        issueOrders'' greatPowers orders game = SomeGame <$> (foldr issueOneOrder return orders game)
          where
            issueOneOrder
                :: (GreatPower, Order (RoundPhase round))
                -> (Game round RoundUnresolved -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) (Game round RoundUnresolved))
                -> (Game round RoundUnresolved -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) (Game round RoundUnresolved))
            issueOneOrder (GreatPower greatPower, order) rest game = case (S.member greatPower greatPowers, order) of
                (False, _) -> throwE NotAllowed
                (True, Order x) -> case issueOrder (align x greatPower) game of
                    Left _ -> throwE (domainReason IssueOrderInvalid)
                    Right x -> rest x

data IssuedOrders where
    Typical :: [(GreatPower, Order Phase.Typical)] -> IssuedOrders
    Retreat :: [(GreatPower, Order Phase.Retreat)] -> IssuedOrders
    Adjust :: [(GreatPower, Order Phase.Adjust)] -> IssuedOrders

deriving instance Generic IssuedOrders
deriving instance Typeable IssuedOrders
instance FromJSON IssuedOrders
instance ToJSON IssuedOrders
instance JSONSchema IssuedOrders where
    schema = gSchema

data IssueOrdersInput = IssueOrdersInput {
      credentials :: Credentials
    , orders :: IssuedOrders
    }

deriving instance Generic IssueOrdersInput
deriving instance Typeable IssueOrdersInput
instance FromJSON IssueOrdersInput
instance ToJSON IssueOrdersInput
instance JSONSchema IssueOrdersInput where
    schema = gSchema

data IssueOrdersOutput = OrdersIssued

deriving instance Generic IssueOrdersOutput
deriving instance Typeable IssueOrdersOutput
instance FromJSON IssueOrdersOutput
instance ToJSON IssueOrdersOutput
instance JSONSchema IssueOrdersOutput where
    schema = gSchema

data IssueOrdersError = IssueOrderInvalid | IssueOrderGameNotStarted

instance ToResponseCode IssueOrdersError where
    toResponseCode _ = 403

deriving instance Generic IssueOrdersError
deriving instance Typeable IssueOrdersError
instance FromJSON IssueOrdersError
instance ToJSON IssueOrdersError
instance JSONSchema IssueOrdersError where
    schema = gSchema
