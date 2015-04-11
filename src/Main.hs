module Main where
 
import Prelude hiding (div, id)
import Haste
import Haste.HPlay.View hiding (head)
import Control.Monad.IO.Class

import Ration.Application
import Ration.Util

main :: IO (Maybe ())
main = do
  addCss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
  addCss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css"
  embedCss myCss
  addJs  "https://code.jquery.com/jquery-1.11.2.min.js"
  addJs  "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"
  embedJs myJs
  runBody $ at "main-content" Insert $ timeout 1000 (runApplication initialState)

addCss :: String -> IO ()
addCss s = addHeader $ 
  link ! atr "rel" "stylesheet" 
       ! href s

embedCss :: String -> IO ()
embedCss s = addHeader $ styleBlock s

addJs :: String -> IO ()
addJs s = addHeader $ script noHtml ! src s

embedJs :: String -> IO ()
embedJs s = addHeader $ script s 

myCss :: String
myCss = ".vertical-align {\n" ++
        "  display: flex;\n" ++
        "  flex-direction: row;\n" ++
        "}\n" ++

        ".vertical-align > [class^=\"col-\"],\n" ++
        ".vertical-align > [class*=\" col-\"] {\n" ++
        "  display: flex;\n" ++
        "  align-items: center;\n" ++
        "  justify-content: center; \n" ++
        "}"

myJs :: String
myJs = "var mouse = {x: 0, y: 0};\n" ++

      "document.addEventListener('mousemove', function(e){\n" ++
      "mouse.x = e.clientX || e.pageX;\n" ++
      "mouse.y = e.clientY || e.pageY\n" ++
      "}, false);"
