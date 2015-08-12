{-|
Module      : Types.Unit
Description : Wrapper for Diplomacy.Unit to make it JSON-able
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Unit (

      Unit(..)

    ) where

import GHC.Generics
import Control.Applicative
import Data.String (fromString)
import Data.Typeable
import qualified Data.Text as T
import Data.Aeson
import Data.JSON.Schema
import qualified Diplomacy.Unit as D
import Text.Parsec

newtype Unit = Unit {
      outUnit :: D.Unit
    }
    deriving (Eq, Ord, Show, Generic, Typeable)

instance ToJSON Unit where
    toJSON (Unit unit) = String . fromString . D.printUnit $ unit

instance FromJSON Unit where
    parseJSON (String txt) = case parse D.parseUnit "" txt of
        Left _ -> empty
        Right x -> return (Unit x)
    parseJSON _ = empty

instance JSONSchema Unit where
    schema _ = Value (LengthBound (Just 1) (Just 1))
