module Ration.Load where

import Prelude hiding (div, id)
import Haste.HPlay.View hiding (head)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Ration.Layout
import Ration.Control.List 
import Ration.Util

loadWidget :: [FoodLayout] -> Maybe Int -> Widget ([FoodLayout], Maybe Int)
loadWidget layouts mselected = do
  (dwidth, dheight) <- liftIO getDocumentSize
  newSelected <- div ! atr "class" "row" <<< do
    div ! atr "class" "col-md-2" <<< return ()
    (_, i) <- div ! atr "class" "col-md-8" <<< do
      -- (zip (foodLayoutName <$> layouts) layouts)
      let test = newFoodLayout "test"
      let items = [("Tbasdb", test), ("T23432", test), ("sfstewsdf", test)]
      listWidget items mselected (max 300 $ fromIntegral dwidth * 0.3, fromIntegral dheight * 0.8)
    div ! atr "class" "col-md-2" <<< return ()
    return i 

  return (layouts, Just $ newSelected) 
