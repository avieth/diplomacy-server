{-|
Module      : Types.ServerOptions
Description : Definition of the server options.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Types.ServerOptions (

      ServerOptions(..)
    , parser

    ) where

import System.FilePath
import Types.Credentials
import Options.Applicative
import Options.Applicative.Builder
import Data.Monoid ((<>))

data ServerOptions = ServerOptions {
      adminUsername :: Username
    , certificateFile :: Maybe FilePath
    , keyFile :: Maybe FilePath
    , host :: String
    , port :: Int
    }
    deriving (Show)

parser :: Parser ServerOptions
parser =
        ServerOptions
    <$> adminUsernameParser
    <*> certificateFileParser
    <*> keyFileParser
    <*> host
    <*> port
  where
    adminUsernameParser = strOption (long "username" <> short 'u' <> help "Username for administration")
    certificateFileParser = optional $ strOption (long "certificate" <> short 'c' <> help "Certificate for TLS")
    keyFileParser = optional $ strOption (long "key" <> short 'k' <> help "Private key for TLS")
    host = strOption (short 'h' <> long "host" <> help "Host address (for bind, and for HTTPS redirect)")
    port = option auto (short 'p' <> long "port" <> value 80 <> help "Port")
