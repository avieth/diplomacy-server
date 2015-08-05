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
{-# LANGUAGE ScopedTypeVariables #-}

module Resources.Order (

      resource

    ) where

import GHC.Generics
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Typeable
import Data.AtLeast
import Data.TypeNat.Vect
import Data.Aeson
import Data.JSON.Schema
import qualified Data.Map as M
import qualified Data.Set as S
import Rest
import Rest.Resource as R
import qualified Diplomacy.Phase as Phase
import Diplomacy.Game as Game
import Diplomacy.Aligned
import qualified Diplomacy.GreatPower as DGP
import Diplomacy.Zone
import Diplomacy.Unit
import Diplomacy.Subject
import qualified Diplomacy.OrderObject as DOO
import qualified Diplomacy.Order as DO
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
            checkAuthorization gameStateView
            (output, nextGame) <- issueOrders' gameStateView
            modifyGameState gameId (return . modifier nextGame)
            return output
        
        modifier :: SomeGame -> GameState -> GameState
        modifier (SomeGame x) state = case state of
            GameNotStarted y duration duration' -> GameNotStarted y duration duration' -- Impossible; should not have this case...
            GameStarted m (AtLeast _ rest) duration duration' elapsed ->
                GameStarted m (AtLeast (VCons (SomeGame x) VNil) rest) duration duration' elapsed

        checkAuthorization :: GameStateView -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) ()
        checkAuthorization gameStateView = case gameStateView of
            GameNotStartedView -> return ()
            GameStartedView greatPowers _ _ _ _ ->
                if issuedOrdersGreatPowers issuedOrders `S.isSubsetOf` (S.map GreatPower greatPowers)
                then return ()
                else throwE NotAllowed

        issueOrders' :: GameStateView -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) (IssueOrdersOutput, SomeGame)
        issueOrders' gameStateView = case gameStateView of
            GameNotStartedView -> throwE (domainReason IssueOrdersGameNotStarted)
            GameStartedView _ (AtLeast (VCons (SomeGame someGame) VNil) _) _ _ _ -> case (someGame, issuedOrders) of
                (TypicalGame TypicalRoundOne Unresolved x y z, Typical os) -> issueOrders'' os someGame
                (RetreatGame RetreatRoundOne Unresolved _ _ _ _ _, Retreat os) -> issueOrders'' os someGame
                (TypicalGame TypicalRoundTwo Unresolved x y z, Typical os) -> issueOrders'' os someGame
                (RetreatGame RetreatRoundTwo Unresolved _ _ _ _ _, Retreat os) -> issueOrders'' os someGame
                (AdjustGame AdjustRound Unresolved _ _ _, Adjust os) -> issueOrders'' os someGame
                _ -> throwE (domainReason (IssueOrdersWrongPhase))

        issueOrders''
            :: forall round .
               [(GreatPower, SomeOrder (RoundPhase round))]
            -> Game round RoundUnresolved
            -> ExceptT (Reason IssueOrdersError) (ReaderT GameId Server) (IssueOrdersOutput, SomeGame)
        issueOrders'' orders game = return $ case game of
            TypicalGame TypicalRoundOne _ _ _ _ -> (IssueOrdersOutputTypical, SomeGame . snd $ Game.issueOrders ordersMap game)
            TypicalGame TypicalRoundTwo _ _ _ _ -> (IssueOrdersOutputTypical, SomeGame . snd $ Game.issueOrders ordersMap game)
            RetreatGame RetreatRoundOne _ _ _ _ _ _ -> (IssueOrdersOutputRetreat, SomeGame . snd $ Game.issueOrders ordersMap game)
            RetreatGame RetreatRoundTwo _ _ _ _ _ _ -> (IssueOrdersOutputRetreat, SomeGame . snd $ Game.issueOrders ordersMap game)
            AdjustGame AdjustRound _ _ _ _ -> (IssueOrdersOutputAdjust, SomeGame . snd $ Game.issueOrders ordersMap game)
          where
            ordersMap :: M.Map Zone (Aligned Unit, DOO.SomeOrderObject (RoundPhase round))
            ordersMap = foldr insertOrder M.empty orders
            insertOrder
                :: (GreatPower, SomeOrder (RoundPhase round))
                -> M.Map Zone (Aligned Unit, DOO.SomeOrderObject (RoundPhase round))
                -> M.Map Zone (Aligned Unit, DOO.SomeOrderObject (RoundPhase round))
            insertOrder (greatPower, order) = case order of
                SomeOrder (DO.SomeOrder (DO.Order (subject, object))) ->
                    M.insert (Zone (subjectProvinceTarget subject)) (align (subjectUnit subject) (outGreatPower greatPower), DOO.SomeOrderObject object)

-- Order inputs look something like this:
--
--     [["England", "F Spa NC - Mid"], ["France", "A Mar - Spa"]]
--
-- It's important that we accept orders for multiple great powers, since one
-- player may control more than one great power.
data IssuedOrders where
    Typical :: [(GreatPower, SomeOrder Phase.Typical)] -> IssuedOrders
    Retreat :: [(GreatPower, SomeOrder Phase.Retreat)] -> IssuedOrders
    Adjust :: [(GreatPower, SomeOrder Phase.Adjust)] -> IssuedOrders

deriving instance Generic IssuedOrders
deriving instance Typeable IssuedOrders
instance FromJSON IssuedOrders
instance ToJSON IssuedOrders
instance JSONSchema IssuedOrders where
    schema = gSchema

-- | The set of all GreatPowers for which at least one order is relevant.
issuedOrdersGreatPowers :: IssuedOrders -> S.Set GreatPower
issuedOrdersGreatPowers orders = case orders of
    Typical os -> foldr (S.insert . fst) S.empty os
    Retreat os -> foldr (S.insert . fst) S.empty os
    Adjust os -> foldr (S.insert . fst) S.empty os

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

-- | TODO throw in the sets of validity criterion, indexed by the relevant
--   order: [["France", "A Mar - Spa SC"], ["MoveUnitCanOccupy"]]
--   And of course, for the adjust phase, take the other set indexed by the
--   relevant great power.
data IssueOrdersOutput where
    IssueOrdersOutputTypical :: IssueOrdersOutput
    IssueOrdersOutputRetreat :: IssueOrdersOutput
    IssueOrdersOutputAdjust :: IssueOrdersOutput

deriving instance Generic IssueOrdersOutput
deriving instance Typeable IssueOrdersOutput
instance FromJSON IssueOrdersOutput
instance ToJSON IssueOrdersOutput
instance JSONSchema IssueOrdersOutput where
    schema = gSchema

-- | We do not consider invalid orders to be an error. If invalid orders
--   are given, the response will reflect it.
data IssueOrdersError
    = IssueOrdersGameNotStarted
    | IssueOrdersWrongPhase

instance ToResponseCode IssueOrdersError where
    toResponseCode _ = 403

deriving instance Generic IssueOrdersError
deriving instance Typeable IssueOrdersError
instance FromJSON IssueOrdersError
instance ToJSON IssueOrdersError
instance JSONSchema IssueOrdersError where
    schema = gSchema
