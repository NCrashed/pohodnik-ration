module Ration.Load where

import Haste.HPlay.View hiding (head)
import Ration.Layout

loadWidget :: [FoodLayout] -> Maybe FoodLayout -> Widget ([FoodLayout], Maybe FoodLayout)
loadWidget layouts mselected = noWidget >> return (layouts, mselected) 