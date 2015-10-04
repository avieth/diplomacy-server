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
import Data.Char (toLower)

newtype GameId = GameId String
    deriving (Show, Generic)

instance FromJSON GameId
instance ToJSON GameId
instance JSONSchema GameId where
    schema = gSchema

instance Eq GameId where
    GameId l == GameId r = fmap toLower l == fmap toLower r

instance Ord GameId where
    GameId l `compare` GameId r = fmap toLower l `compare` fmap toLower r
