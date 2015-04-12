module Ration.Control.List(
    listWidget
  ) where

import Prelude hiding (div, id)
import Haste hiding (style)
import Haste.Graphics.Canvas
import Haste.HPlay.View hiding (head)
import Control.Monad
import Control.Monad.IO.Class
import Ration.Util

listWidget :: [(String, a)] -> Maybe Int -> (Double, Double) -> Widget (a, Int)
listWidget items msel size@(width, height) = do 
  canvasId <- fmap ("canvas" ++) getNextId
  resetEventData
  wraw (do
    canvas ! id canvasId
           ! style "border: 1px solid black;" 
           ! atr "width" (show width)
           ! atr "height" (show height)
           $ noHtml)
    `fire` OnClick

  wraw $ liftIO $ do 
    wcan <- getCanvasById canvasId
    case wcan of 
      Nothing -> return ()
      Just can -> render can $ drawList (fst <$> items) msel size

  e@(EventData typ _) <- getEventData
  evdata <- continueIf (evtName OnClick == typ) e

  offset <- liftIO $ getElementPosition $ "#" ++ canvasId
  mousePos <- liftIO getMousePosition
  let item = toItemIndex offset mousePos
  when (item < 0 || item >= length items) noWidget
  
  return (snd $ items !! item, item)
  where
    toItemIndex :: (Int, Int) -> (Int, Int) -> Int
    toItemIndex (offx, offy) (x, y) = if dx >= 0 && dy >= 0 && dx <= width && dy <= height 
      then floor $ dy / (rowHeight * height)
      else -1
      where
        dx = fromIntegral $ x - offx
        dy = fromIntegral $ y - offy

drawList :: [String] -> Maybe Int -> (Double, Double) -> Picture ()
drawList items msel (width, height) = do
  maybe (return ()) drawSelected msel
  mapM_ drawLine $ zip [0 .. length items - 1] items
  where
    drawLine (i, s) = 
      let margin = rowHeight * 0.2
          newHeight = (rowHeight - 2*margin) * height
          scl = newHeight / textHeight
      in translate (0, (fromIntegral (i+1) * rowHeight - margin) * height ) $ scale (scl, scl) $ text (0, 0) s

    drawSelected i = color (RGB 230 230 250) $  
      translate (0, fromIntegral i * rowHeight * height) $ fill $ rect (0, 0) (width, rowHeight*height)

rowHeight :: Double
rowHeight = 0.05

textHeight :: Double
textHeight = 7.5