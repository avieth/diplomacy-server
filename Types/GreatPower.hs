{-|
Module      : Types.GreatPower
Description : Wrapper for Diplomacy.GreatPower to make it JSON-able
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.GreatPower (

      GreatPower(..)

    ) where

import GHC.Generics
import Control.Applicative
import Data.String (fromString)
import Data.Typeable
import qualified Data.Text as T
import Data.Aeson
import Data.JSON.Schema
import qualified Diplomacy.GreatPower as D

newtype GreatPower = GreatPower D.GreatPower
    deriving (Eq, Ord, Show, Generic, Typeable)

instance ToJSON GreatPower where
    toJSON (GreatPower p) = String . fromString . show $ p

instance FromJSON GreatPower where
    parseJSON (String txt) = case reads (T.unpack txt) of
        (greatPower, _) : _ -> return (GreatPower greatPower)
        _ -> empty
    parseJSON _ = empty

instance JSONSchema GreatPower where
    schema _ = Value (LengthBound Nothing Nothing)
