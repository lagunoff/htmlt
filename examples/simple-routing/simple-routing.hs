import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Functor
import HtmlT
import Wasm.Compat.Prim
import Wasm.Compat.Marshal

import "this" Pages
import "this" Router
import "this" Utils
import "this" Assets

main :: IO ()
main = return ()

foreign export ccall wasm_main :: IO ()
wasm_main = void $ attachToBody do
  liftIO $ insertScript prismJs
  el "style" (text awsmCss)
  el "style" (text customCss)
  el "style" (text prismCss)
  urlHashRef <- mkUrlHashRef
  let routeDyn = fromMaybe HomeR . fromUrl <$> fromRef urlHashRef
  header_ do
    h1_ "Simple in-browser routing example"
    p_ do
      "See the source on the "
      a_ [href_ "https://github.com/lagunoff/htmlt/blob/master/examples/\
        \simple-routing/"] "github"
    nav_ $ ul_ do
      let link t r = li_ $ a_ [href_ (toUrl r)] $ text t
      link "Home" HomeR
      link "List of Countries" $ CountriesListR defaultCountriesListQ
      link "Countries on the Map" $ CountriesMapR defaultCountriesMapQ
  main_ $ dyn $ routeDyn <&> \case
    HomeR -> homePage
    CountriesMapR q -> countriesMapPage q
    CountriesListR q -> countriesListPage q
  footer_ $ p_ $ a_ [href_ "https://github.com/lagunoff"] "Vladislav Lagunov"

customCss :: JSString
customCss = "\
  \body header, body main, body footer, body article {\
  \  max-width: 80rem;\
  \}\
  \pre {\
  \  border-left: solid 8px rgb(0 0 0 / 14%);\
  \  padding-left: 16px;\
  \  background: transparent;\
  \}\
  \.CountriesList table {\
  \  width: 100%;;\
  \}\
  \.CountriesList table th, .CountriesList table td {\
  \  white-space: nowrap;\
  \}\
  \.CountriesList table th:last-child, .CountriesList table td:last-child {\
  \  width: 99%;\
  \  text-align: right;\
  \}\
  \.CountriesMap svg path.selected {\
  \  fill: #bfd3ff !important;\
  \  stroke: #4175e8;\
  \  stroke-width: 1;\
  \}\
  \.CountriesMap svg path {\
  \  cursor: pointer;\
  \}\
  \.CountriesMap svg path:hover {\
  \  fill: #ccc;\
  \}"
