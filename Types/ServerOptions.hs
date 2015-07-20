{-|
Module      : 
Description : 
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

data ServerOptions = ServerOptions {
      adminUsername :: Username
    , adminPassword :: Password
    , certificateFile :: FilePath
    , keyFile :: FilePath
    , port :: Int
    }
    deriving (Show)

parser :: Parser ServerOptions
parser =
        ServerOptions
    <$> adminUsernameParser
    <*> adminPasswordParser
    <*> certificateFileParser
    <*> keyFileParser
    <*> port
  where
    adminUsernameParser = strOption (long "username" <> short 'u' <> help "Username for administration")
    adminPasswordParser = strOption (long "password" <> short 'p' <> help "Password for administration")
    certificateFileParser = strOption (long "certificate" <> short 'c' <> value "certificate.pem" <> help "Certificate for TLS")
    keyFileParser = strOption (long "key" <> short 'k' <> value "key.pem" <> help "Private key for TLS")
    port = option auto (long "port" <> value 4347 <> help "Port on which to run the server")


