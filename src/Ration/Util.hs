{-# LANGUAGE OverloadedStrings #-}
module Ration.Util(
    getElementPosition
  , styleBlock
  , getMousePosition
  , getDocumentSize
  , cbutton
  , cbuttonM
  , timeout
  , clearTimers
  , wloop
  , panel
  , labelRow
  , binToInt
  , binToDouble
  , debugMsg
  , eitherWidget
  , saveText
  ) where

import Prelude hiding (div, id)
import qualified Prelude
import Haste
import Haste.Foreign
import Haste.App (MonadIO)
import Haste.Prim
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Monoid
import Haste.Perch (ToElem, Perch, nelem, child, span)
import Haste.HPlay.View hiding (head)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Unsafe.Coerce 

newtype JQuery = JQuery JSAny
newtype JPosition = JPosition JSAny
newtype JMouse = JMouse JSAny
newtype JDocument = JDocument JSAny 

jsJquery :: JSString -> IO JQuery
jsJquery v = JQuery <$> ffi "(function(e) {return $(e);})" v

jsOffset :: JQuery -> IO JPosition
jsOffset (JQuery jsany) = JPosition <$> ffi "(function(jq) {return jq.offset();})" jsany

jsLeft :: JPosition -> IO Int
jsLeft (JPosition jsany) = ffi "(function(jp) {return jp.left;})" jsany

jsTop :: JPosition -> IO Int
jsTop (JPosition jsany) = ffi "(function(jp) {return jp.top;})" jsany

jsMouse :: IO JMouse
jsMouse = JMouse <$> ffi "(function() {return mouse;})"

jsMouseX :: JMouse -> IO Int
jsMouseX (JMouse jsany) = ffi "(function(m) {return m.x;})" jsany

jsMouseY :: JMouse -> IO Int
jsMouseY (JMouse jsany) = ffi "(function(m) {return m.y;})" jsany

jsDocument :: IO JDocument
jsDocument = JDocument <$> ffi "(function() {return document.documentElement;})"

jsPageScrollX :: JDocument -> IO Int 
jsPageScrollX (JDocument jsany) = 
  ffi "(function(doc) {return (window.pageXOffset || doc.scrollLeft) - (doc.clientLeft || 0);})" jsany

jsPageScrollY :: JDocument -> IO Int 
jsPageScrollY (JDocument jsany) = 
  ffi "(function(doc) {return (window.pageYOffset || doc.scrollTop)  - (doc.clientTop || 0);})" jsany

jsDocumentWidth :: IO Int 
jsDocumentWidth = ffi "(function() {return $(document).width();})"

jsDocumentHeight :: IO Int 
jsDocumentHeight = ffi "(function() {return $(document).height();})"

jsSetOnPageLoad :: JSFun a -> IO ()
jsSetOnPageLoad (JSFun f) = ffi "(function (c) { document.onload = c; })" 
  $ (unsafeCoerce f :: JSAny)

-- | Since we can't name it '$', let's just call it 'j'.
j :: JSString -> (JQuery -> IO a) -> IO a
j s action = jsJquery s >>= action

-- | Returns element position
getElementPosition :: String -> IO (Int, Int)
getElementPosition sel = j (toJSStr sel) $ \jq -> do
  pos <- jsOffset jq
  xpos <- jsLeft pos
  ypos <- jsTop pos
  return (xpos, ypos) 

getMousePosition :: IO (Int, Int)
getMousePosition = do
  m <- jsMouse
  x <- jsMouseX m 
  y <- jsMouseY m
  doc <- jsDocument
  sx <- jsPageScrollX doc
  sy <- jsPageScrollY doc 
  return (x+sx, y+sy)

getDocumentSize :: IO (Int, Int)
getDocumentSize = do 
  x <- jsDocumentWidth
  y <- jsDocumentHeight
  return (x, y)

styleBlock :: ToElem a => a -> Perch 
styleBlock cont = nelem  "style" `child` cont

-- | active button. When clicked, return the first parameter
cbutton :: a -> String -> Widget a
cbutton x slabel= static $ do
        button slabel ! id slabel ! atr "class" "btn btn-primary" ! atr "type" "button" ! atr "style" "margin-right: 10px; margin-left: 10px; margin-top: 3px" `pass` OnClick
        return x
      `continuePerch` slabel

cbuttonM :: IO a -> String -> Widget a
cbuttonM x slabel= static $ do
        button slabel ! id slabel ! atr "class" "btn btn-primary" ! atr "type" "button" ! atr "style" "margin-right: 10px; margin-left: 10px; margin-top: 3px" `pass` OnClick
        liftIO x
      `continuePerch` slabel

timeoutStore :: IORef [String] 
timeoutStore = unsafePerformIO $ newIORef []

storeTimeout :: String -> IO ()
storeTimeout ids = do
  idss <- readIORef timeoutStore
  writeIORef timeoutStore $ ids : idss

removeTimeout :: String -> IO ()
removeTimeout ids = do
  idss <- readIORef timeoutStore
  writeIORef timeoutStore $ filter (/= ids) idss

hasTimeout :: String -> IO Bool
hasTimeout ids = elem ids <$> readIORef timeoutStore

peekTimeout :: String -> Widget () 
peekTimeout ids = do 
  idss <- liftIO $ readIORef timeoutStore
  case ids `elem` idss of 
    True -> noWidget 
    False -> return ()

clearTimers :: IO ()
clearTimers = writeIORef timeoutStore []

timeout :: Int -> Widget a -> Widget a
timeout mss wa = do 
  id <- genNewId
  liftIO $ storeTimeout id

  cont <- getCont
  ht <- liftIO $ hasTimeout id
  when ht $ setTimeout mss $ do
    ht' <- liftIO $ hasTimeout id
    when ht' $ do 
      removeTimeout id
      runCont cont

  peekTimeout id 
  wa

wloop :: a -> (a -> Widget a) -> Widget ()
wloop initialState wa = View $ do
  nid <- genNewId

  FormElm form mx <- runView $ go nid initialState 
    
  return $ FormElm ((Haste.Perch.span ! atr "id" nid $ noHtml) <> form) mx
  where
    go nid state = do 
      nextState <- at nid Insert (wa state)
      go nid nextState

panel :: String -> Perch -> Perch
panel ts bd = div ! atr "class" "panel panel-default" $ mconcat [
    div ! atr "class" "panel-heading" $ h3 ! atr "class" "panel-title" $ toJSString ts
  , div ! atr "class" "panel-body" $ bd
  ]

labelRow :: Int -> String -> String -> Perch
labelRow ri ts vs = div ! atr "class" "row" $ mconcat [
    div ! atr "class" (colClass ri) $ label $ toJSString ts
  , div ! atr "class" (colClass (12-ri)) $ toJSString vs
  ]
  where colClass i = "col-md-" ++ show i

binToInt :: [Int] -> Int
binToInt chromosome = binToInt' 0 base chromosome
    where base = 2^(length chromosome-1)

binToInt':: Int -> Int -> [Int] -> Int
binToInt' acc 0 [x] = acc+x
binToInt' acc base (x:xs) = binToInt' (acc+x*base) newBase xs
    where newBase = Prelude.div base 2
binToInt' acc _ _ = acc

binToDouble:: Double -> [Int] -> Double
binToDouble n chromosome = binToDouble' 0 base chromosome
    where base = 2**(n-1) -- | n = 1 means 1 digit before "."; means (2^0)*digit

binToDouble':: Double -> Double -> [Int] -> Double
binToDouble' acc _ [] = acc
binToDouble' acc base (x:xs) = binToDouble' newAcc newBase xs
    where 
        newAcc = acc + (fromIntegral x)*base
        newBase = base / 2.0

debugMsg :: (MonadIO m, Show a) => a -> m ()
debugMsg = liftIO . writeLog . show 

eitherWidget :: Widget a -> Widget b -> Widget (Either a b)
eitherWidget wa wb = (return . Left =<< wa) <|> (return . Right =<< wb)

saveText :: String -> String -> IO ()
saveText = ffi "(function(str, filename) {saveText(str, filename);})"