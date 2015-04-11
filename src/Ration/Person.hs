{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Ration.Person where

import Data.Typeable
import Control.Applicative
import Haste.JSON
import Haste.Serialize

data Person = Person {
    personName :: String
  , personSurname :: Maybe String
  , personPatronim :: Maybe String
} deriving (Typeable, Show)

instance Serialize Person where
  toJSON v = Dict $ [
      ("personName", toJSON $ personName v) ]
    ++ (maybe [] (\v' -> [("personSurname", toJSON v')]) (personSurname v)) ++
       (maybe [] (\v' -> [("personPatronim", toJSON v')]) (personPatronim v)) ++
    []

  parseJSON j = Person 
    <$> j .: "personName"
    <*> j .:? "personSurname"
    <*> j .:? "personPatronim"