module Ration.Application where

import Prelude hiding (div, id)
import Haste.HPlay.View hiding (head)
import Haste.Serialize (toJSON)
import Control.Monad.IO.Class
import Ration.Layout
import Ration.Util
import Ration.Load 

data ApplicationState =
    AppLoad [FoodLayout] (Maybe FoodLayout)
  | AppMain (Maybe FoodLayout)

data Route = 
    RouteLoad
  | RouteSave
  | RouteMainCreate
  | RouteMainLoad
  deriving (Show)

initialState :: ApplicationState
initialState = AppLoad [] Nothing

runApplication :: ApplicationState -> Widget ()
runApplication state = wloop state go
  where
    go :: ApplicationState -> Widget ApplicationState
    go localState@(AppLoad layouts selected) = handleRouting localState (loadWidget layouts selected) routing calc
      where
        routing RouteMainCreate = return $ AppMain Nothing 
        routing RouteMainLoad = return $ AppMain selected
        routing RouteSave = case selected of 
            Just s -> do
              liftIO $ saveText (show $ toJSON s) "layout.json"
              return localState
            Nothing -> return localState
        routing route = fail $ "invalid route in config state " ++ show route

        calc (newLayouts, newSelected) = return $ AppLoad newLayouts newSelected

handleRouting :: ApplicationState -> Widget a -> (Route -> Widget ApplicationState) -> (a -> Widget ApplicationState) -> Widget ApplicationState
handleRouting localState wa routing calc = do
  update <- eitherWidget wa $ routeWidget localState
  case update of
    Right route -> routing route
    Left newVal -> calc newVal

routeWidget :: ApplicationState -> Widget Route
routeWidget state = div ! atr "class" "row" 
  <<< div ! atr "class" "col-md-4 col-md-offset-5"
  <<< go state
  where
    go (AppLoad _ Nothing) = bigBtn RouteMainCreate "Создать раскладку"
    go (AppLoad _ (Just _)) = 
          bigBtn RouteMainLoad "Открыть раскладку" 
      <|> bigBtn RouteSave "Сохранить раскладку" 
      <|> bigBtn RouteMainCreate "Создать раскладку"
    go (AppMain {}) = bigBtn RouteLoad "Назад"

    bigBtn v s = cbutton v s <! [atr "class" "btn btn-primary btn-lg"]