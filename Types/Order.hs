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

      Order(..)

    , parseOrderTypical
    , parseOrderRetreat
    , parseOrderAdjust

    , printOrder
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
import Diplomacy.Order hiding (Order(..))
import qualified Diplomacy.Order as DO
import Diplomacy.OrderObject
import Diplomacy.Subject
import Diplomacy.Province
import Diplomacy.Unit

data Order (phase :: Phase) where
    Order :: DO.Order phase order -> Order phase

deriving instance Show (Order phase)

instance JSONSchema (Order phase) where
    schema _ = Value (LengthBound Nothing Nothing)

instance ToJSON (Order phase) where
    toJSON = String . printOrder

instance FromJSON (Order Typical) where
    parseJSON (String txt) = case parse parseOrderTypical "" txt of
        Left _ -> empty
        Right x -> return x
    parseJSON _ = empty

instance FromJSON (Order Retreat) where
    parseJSON (String txt) = case parse parseOrderRetreat "" txt of
        Left _ -> empty
        Right x -> return x
    parseJSON _ = empty

instance FromJSON (Order Adjust) where
    parseJSON (String txt) = case parse parseOrderAdjust "" txt of
        Left _ -> empty
        Right x -> return x
    parseJSON _ = empty

printOrder :: Order phase -> T.Text
printOrder (Order order) = T.concat [
      printSubject (orderSubject order)
    , " "
    , printObject (orderSubject order) (orderObject order)
    ]

printSubject :: Subject -> T.Text
printSubject (unit, pt) = T.concat [
      printUnit unit
    , " "
    , printProvinceTarget pt
    ]

printObject :: Subject -> OrderObject phase order -> T.Text
printObject subject object = case object of
    MoveObject pt ->
        if subjectProvinceTarget subject == pt
        then "Hold"
        else T.concat ["- ", printProvinceTarget pt]
    SupportObject subj pt ->
        if subjectProvinceTarget subj == pt
        then T.concat ["S ", printSubject subj]
        else T.concat ["S ", printSubject subj, " - ", printProvinceTarget pt]
    ConvoyObject subj pt ->
        T.concat ["C ", printSubject subj, " - ", printProvinceTarget pt]
    SurrenderObject -> "Surrender"
    WithdrawObject pt ->
        T.concat ["- ", printProvinceTarget pt]
    DisbandObject -> "Disband"
    BuildObject -> "Build"
    ContinueObject -> "Continue"

parseOrderTypical :: Parser (Order Typical)
parseOrderTypical = do
    subject <- parseSubject
    spaces
    SomeOrderObject object <- parseObjectTypical subject
    return $ Order (DO.Order (subject, object))

parseOrderRetreat :: Parser (Order Retreat)
parseOrderRetreat = do
    subject <- parseSubject
    spaces
    SomeOrderObject object <- parseObjectRetreat
    return $ Order (DO.Order (subject, object))

parseOrderAdjust :: Parser (Order Adjust)
parseOrderAdjust = do
    subject <- parseSubject
    spaces
    SomeOrderObject object <- parseObjectAdjust
    return $ Order (DO.Order (subject, object))

parseSubject :: Parser Subject
parseSubject = do
    unit <- parseUnit
    spaces
    pt <- parseProvinceTarget
    return (unit, pt)

parseObjectTypical :: Subject -> Parser (SomeOrderObject Typical)
parseObjectTypical subject =
        (SomeOrderObject <$> try (parseHold subject))
    <|> (SomeOrderObject <$> try parseMove)
    <|> (SomeOrderObject <$> try parseSupport)
    <|> (SomeOrderObject <$> try parseConvoy)

parseObjectRetreat :: Parser (SomeOrderObject Retreat)
parseObjectRetreat =
        (SomeOrderObject <$> try parseSurrender)
    <|> (SomeOrderObject <$> try parseWithdraw)

parseObjectAdjust :: Parser (SomeOrderObject Adjust)
parseObjectAdjust =
        (SomeOrderObject <$> try parseDisband)
    <|> (SomeOrderObject <$> try parseBuild)
    <|> (SomeOrderObject <$> try parseContinue)

parseHold :: Subject -> Parser (OrderObject Typical Move)
parseHold subject = (string "Hold" <|> string "hold") *> object
  where
    object = pure (MoveObject (subjectProvinceTarget subject))

parseMove :: Parser (OrderObject Typical Move)
parseMove = do
    char '-'
    spaces
    pt <- parseProvinceTarget
    return $ MoveObject pt

parseSupport :: Parser (OrderObject Typical Support)
parseSupport = do
    char 'S'
    spaces
    subject <- parseSubject
    target <- Text.Parsec.option (subjectProvinceTarget subject) (try rest)
    return $ SupportObject subject target
  where
    rest = do
        spaces
        char '-'
        spaces
        parseProvinceTarget

parseConvoy :: Parser (OrderObject Typical Convoy)
parseConvoy = do
    char 'C'
    spaces
    subject <- parseSubject
    spaces
    char '-'
    spaces
    target <- parseProvinceTarget
    return $ ConvoyObject subject target

parseSurrender :: Parser (OrderObject Retreat Surrender)
parseSurrender = do
    string "Surrender"
    return $ SurrenderObject

parseWithdraw :: Parser (OrderObject Retreat Withdraw)
parseWithdraw = do
    char '-'
    spaces
    pt <- parseProvinceTarget
    return $ WithdrawObject pt

parseDisband :: Parser (OrderObject Adjust Disband)
parseDisband = do
    string "Disband"
    return $ DisbandObject

parseBuild :: Parser (OrderObject Adjust Build)
parseBuild = do
    string "Build"
    return $ BuildObject

parseContinue :: Parser (OrderObject Adjust Continue)
parseContinue = do
    string "Continue"
    return $ ContinueObject
