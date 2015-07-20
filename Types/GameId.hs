{-|
Module      : Types.GameId
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.GameId (

      GameId(..)

    ) where

import GHC.Generics
import Data.Typeable
import Data.Aeson
import Data.JSON.Schema

newtype GameId = GameId String
    deriving (Eq, Ord, Show, Generic)

instance FromJSON GameId
instance ToJSON GameId
instance JSONSchema GameId where
    schema = gSchema
