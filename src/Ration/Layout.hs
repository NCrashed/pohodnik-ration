{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Ration.Layout where

import Haste.JSON
import Haste.Serialize
import Data.Time.Calendar
import Data.Time.Format
import Data.Maybe
import Data.Typeable
import Control.Applicative
import Ration.Person 
import Ration.Food 

data FoodLayout = FoodLayout {
    foodLayoutName :: String
  , foodLayoutDate :: Maybe Day
  , foodLayoutPersons :: [Person]
  , foodLayoutDays :: [LayoutDay]
  , foodLayoutFood :: [Food]
} deriving (Typeable, Show, Eq)

instance Serialize FoodLayout where
  toJSON v = Dict $ [
      ("foodLayoutName", toJSON $ foodLayoutName v) ]
    ++ (maybe [] (\v' -> [("foodLayoutDate", toJSON $ showDay v')]) (foodLayoutDate v)) ++
    [ ("foodLayoutPersons", toJSON $ foodLayoutPersons v)
    , ("foodLayoutDays", toJSON $ foodLayoutDays v)
    , ("foodLayoutFood", toJSON $ foodLayoutFood v)
    ]
    where showDay = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

  parseJSON j = FoodLayout 
    <$> j .: "foodLayoutName"
    <*> (fmap readDay <$> j .:? "foodLayoutDate")
    <*> j .: "foodLayoutPersons"
    <*> j .: "foodLayoutDays"
    <*> j .: "foodLayoutFood"
    where readDay = parseTimeOrError True defaultTimeLocale (iso8601DateFormat Nothing)

data LayoutDay = LayoutDay
  deriving (Typeable, Show, Eq)

instance Serialize LayoutDay where
  toJSON v = Dict $ [
    ]

  parseJSON j = pure LayoutDay

newFoodLayout :: String -> FoodLayout
newFoodLayout name = FoodLayout {
    foodLayoutName = name
  , foodLayoutDate = Nothing
  , foodLayoutPersons = []
  , foodLayoutDays = []
  , foodLayoutFood = []
  }