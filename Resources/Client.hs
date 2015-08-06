{-|
Module      : Resources.Client
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
{-# LANGUAGE RankNTypes #-}

module Resources.Client (

      resource

    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State as S
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import qualified Data.Map as M
import Data.Hourglass
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Rest
import Rest.Resource as R
import Types.Server
import Types.GameId
import Types.Credentials
import Types.GameState
import Resources.Game.Create as Create
import Resources.Game.Remove as Remove

resource :: Resource Server Server () Void Void
resource = (mkResource enter)
    { R.name = "diplomacy"
    , R.schema = Rest.singleton () (named [])
    , R.get = Just get
    }
  where

    enter :: forall b . () -> Server b -> Server b
    enter _ = id

    get :: Handler Server
    get = secureHandler $ mkConstHandler (jsonE . fileO) $ doGet

    doGet :: ExceptT (Reason Void) Server (BL.ByteString, String, Bool)
    doGet = do
        state <- lift S.get
        return $ (clientHtml state, "client.html", False)
