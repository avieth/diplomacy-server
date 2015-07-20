{-|
Module      : Types.UserData
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Types.UserData (

      UserData(..)

    ) where

import Types.Credentials (Password)
import Diplomacy.GreatPower

data UserData f = UserData {
      password :: Password
    , greatPower :: f GreatPower
    }
