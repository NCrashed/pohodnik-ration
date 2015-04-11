module Ration.Layout where

import Data.Time.Calendar
import Ration.Person 
import Ration.Food 

data FoodLayout = FoodLayout {
    foodLayoutName :: String
  , foodLayoutDate :: Maybe Day
  , foodLayoutPersons :: [Person]
  , foodLayoutDays :: [LayoutDay]
  , foodLayoutFood :: [Food]
}

data LayoutDay = LayoutDay