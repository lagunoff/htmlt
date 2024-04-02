import Clickable.Main.DevServer

import "this" JsMain (jsMain)

main :: IO ()
main = runServer jsMain
