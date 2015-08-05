{-|
Module      : Types.Order
Description : Wrapper for Diplomacy.Order making to JSON-able.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types.Order (

      SomeOrder(..)
    , SomeOrderObject(..)

    , parseSomeOrderTypical
    , parseSomeOrderRetreat
    , parseSomeOrderAdjust

    , printSomeOrder
    , printObject

    ) where

import Control.Applicative
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Parser
import Data.JSON.Schema
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import Diplomacy.Phase
import Diplomacy.OrderType
import Diplomacy.Order hiding (Order(..), SomeOrder(..))
import qualified Diplomacy.Order as DO
import qualified Diplomacy.OrderObject as DOO
import Diplomacy.Subject
import Diplomacy.Province
import Diplomacy.Unit

newtype SomeOrderObject (phase :: Phase) = SomeOrderObject {
      outSomeOrderObject :: DOO.SomeOrderObject phase
    }

deriving instance Show (SomeOrderObject phase)

instance JSONSchema (SomeOrderObject phase) where
    schema _ = Value (LengthBound Nothing Nothing)

instance ToJSON (SomeOrderObject phase) where
    toJSON (SomeOrderObject (DOO.SomeOrderObject object)) = String (printObject object)

instance FromJSON (SomeOrderObject Typical) where
    parseJSON (String txt) = case parse parseObjectTypical "" txt of
        Left _ -> empty
        Right x -> return (SomeOrderObject x)
    parseJSON _ = empty

instance FromJSON (SomeOrderObject Retreat) where
    parseJSON (String txt) = case parse parseObjectRetreat "" txt of
        Left _ -> empty
        Right x -> return (SomeOrderObject x)
    parseJSON _ = empty

instance FromJSON (SomeOrderObject Adjust) where
    parseJSON (String txt) = case parse parseObjectAdjust "" txt of
        Left _ -> empty
        Right x -> return (SomeOrderObject x)
    parseJSON _ = empty

newtype SomeOrder (phase :: Phase) = SomeOrder {
      outSomeOrder :: DO.SomeOrder phase
    }

deriving instance Show (SomeOrder phase)
deriving instance Eq (SomeOrder phase)
deriving instance Ord (SomeOrder phase)

instance JSONSchema (SomeOrder phase) where
    schema _ = Value (LengthBound Nothing Nothing)

instance ToJSON (SomeOrder phase) where
    toJSON = String . printSomeOrder

instance FromJSON (SomeOrder Typical) where
    parseJSON (String txt) = case parse parseSomeOrderTypical "" txt of
        Left _ -> empty
        Right x -> return x
    parseJSON _ = empty

instance FromJSON (SomeOrder Retreat) where
    parseJSON (String txt) = case parse parseSomeOrderRetreat "" txt of
        Left _ -> empty
        Right x -> return x
    parseJSON _ = empty

instance FromJSON (SomeOrder Adjust) where
    parseJSON (String txt) = case parse parseSomeOrderAdjust "" txt of
        Left _ -> empty
        Right x -> return x
    parseJSON _ = empty

printSomeOrder :: SomeOrder phase -> T.Text
printSomeOrder (SomeOrder (DO.SomeOrder order)) = T.concat [
      printSubject (orderSubject order)
    , " "
    , printObject (orderObject order)
    ]

printSubject :: Subject -> T.Text
printSubject (unit, pt) = T.concat [
      printUnit unit
    , " "
    , printProvinceTarget pt
    ]

printObject :: DOO.OrderObject phase order -> T.Text
printObject object = case object of
    DOO.MoveObject pt -> T.concat ["- ", printProvinceTarget pt]
    DOO.SupportObject subj pt ->
        if subjectProvinceTarget subj == pt
        then T.concat ["S ", printSubject subj]
        else T.concat ["S ", printSubject subj, " - ", printProvinceTarget pt]
    DOO.ConvoyObject subj pt ->
        T.concat ["C ", printSubject subj, " - ", printProvinceTarget pt]
    DOO.SurrenderObject -> "Surrender"
    DOO.WithdrawObject pt ->
        T.concat ["- ", printProvinceTarget pt]
    DOO.DisbandObject -> "Disband"
    DOO.BuildObject -> "Build"
    DOO.ContinueObject -> "Continue"

parseSomeOrderTypical :: Parser (SomeOrder Typical)
parseSomeOrderTypical = do
    subject <- parseSubject
    spaces
    DOO.SomeOrderObject object <- parseObjectTypical
    return $ SomeOrder (DO.SomeOrder (DO.Order (subject, object)))

parseSomeOrderRetreat :: Parser (SomeOrder Retreat)
parseSomeOrderRetreat = do
    subject <- parseSubject
    spaces
    DOO.SomeOrderObject object <- parseObjectRetreat
    return $ SomeOrder (DO.SomeOrder (DO.Order (subject, object)))

parseSomeOrderAdjust :: Parser (SomeOrder Adjust)
parseSomeOrderAdjust = do
    subject <- parseSubject
    spaces
    DOO.SomeOrderObject object <- parseObjectAdjust
    return $ SomeOrder (DO.SomeOrder (DO.Order (subject, object)))

parseSubject :: Parser Subject
parseSubject = do
    unit <- parseUnit
    spaces
    pt <- parseProvinceTarget
    return (unit, pt)

parseObjectTypical :: Parser (DOO.SomeOrderObject Typical)
parseObjectTypical =
        (DOO.SomeOrderObject <$> try parseMove)
    <|> (DOO.SomeOrderObject <$> try parseSupport)
    <|> (DOO.SomeOrderObject <$> try parseConvoy)

parseObjectRetreat :: Parser (DOO.SomeOrderObject Retreat)
parseObjectRetreat =
        (DOO.SomeOrderObject <$> try parseSurrender)
    <|> (DOO.SomeOrderObject <$> try parseWithdraw)

parseObjectAdjust :: Parser (DOO.SomeOrderObject Adjust)
parseObjectAdjust =
        (DOO.SomeOrderObject <$> try parseDisband)
    <|> (DOO.SomeOrderObject <$> try parseBuild)
    <|> (DOO.SomeOrderObject <$> try parseContinue)

parseMove :: Parser (DOO.OrderObject Typical Move)
parseMove = do
    char '-'
    spaces
    pt <- parseProvinceTarget
    return $ DOO.MoveObject pt

parseSupport :: Parser (DOO.OrderObject Typical Support)
parseSupport = do
    char 'S'
    spaces
    subject <- parseSubject
    target <- Text.Parsec.option (subjectProvinceTarget subject) (try rest)
    return $ DOO.SupportObject subject target
  where
    rest = do
        spaces
        char '-'
        spaces
        parseProvinceTarget

parseConvoy :: Parser (DOO.OrderObject Typical Convoy)
parseConvoy = do
    char 'C'
    spaces
    subject <- parseSubject
    spaces
    char '-'
    spaces
    target <- parseProvinceTarget
    return $ DOO.ConvoyObject subject target

parseSurrender :: Parser (DOO.OrderObject Retreat Surrender)
parseSurrender = do
    string "Surrender"
    return $ DOO.SurrenderObject

parseWithdraw :: Parser (DOO.OrderObject Retreat Withdraw)
parseWithdraw = do
    char '-'
    spaces
    pt <- parseProvinceTarget
    return $ DOO.WithdrawObject pt

parseDisband :: Parser (DOO.OrderObject Adjust Disband)
parseDisband = do
    string "Disband"
    return $ DOO.DisbandObject

parseBuild :: Parser (DOO.OrderObject Adjust Build)
parseBuild = do
    string "Build"
    return $ DOO.BuildObject

parseContinue :: Parser (DOO.OrderObject Adjust Continue)
parseContinue = do
    string "Continue"
    return $ DOO.ContinueObject
