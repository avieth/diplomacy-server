{-|
Module      : Types.Credentials
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
{-# LANGUAGE FlexibleContexts #-}

module Types.Credentials (

      Username
    , Password
    , Credentials(..)

    ) where

import GHC.Generics
import Data.Typeable
import Data.Aeson
import Data.JSON.Schema

type Username = String
type Password = String

data Credentials = Credentials {
      username :: Username
    , password :: Password
    }
    deriving (Eq, Generic, Typeable)

instance FromJSON Credentials
instance ToJSON Credentials
instance JSONSchema Credentials where
    schema = gSchema
