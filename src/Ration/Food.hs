{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Ration.Food where

import Data.Typeable
import Control.Applicative
import Haste.JSON
import Haste.Serialize

data Food = Food {
    foodName :: String
  , foodDescr :: String
  }
  deriving (Typeable, Show)


instance Serialize Food where
  toJSON v = Dict $ [
      ("foodName", toJSON $ foodName v) 
    , ("foodDescr", toJSON $ foodDescr v)
    ]

  parseJSON j = Food 
    <$> j .: "foodName"
    <*> j .: "foodDescr"