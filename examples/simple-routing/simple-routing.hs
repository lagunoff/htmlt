import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text
import HtmlT

import "this" Pages
import "this" Router
import "this" Utils
import "this" Assets

main :: IO ()
main = do
  env <- newReactiveEnv
  urlHashRef <- mkUrlHashRef env
  bodyEl <- getCurrentBody
  let routeDyn = fromMaybe HomeR . fromUrl <$> fromRef urlHashRef
  let startOpts = StartOpts env bodyEl
  void $ startWithOptions startOpts do
    el "style" (text awsmCss)
    el "style" (text customCss)
    el "style" (text prismCss)
    header_ do
      h1_ "Simple in-browser routing example"
      p_ do
        "See the source on the "
        a_ [href_ "https://github.com/lagunoff/htmlt/blob/master/examples/simple-routing/"]
          "github"
      nav_ $ ul_ do
        let link t r = li_ $ a_ [href_ (toUrl r)] $ text t
        link "Home" HomeR
        link "List of Countries" $ CountriesListR defaultCountriesListQ
        link "Countries on the Map" $ CountriesMapR defaultCountriesMapQ
    main_ do
      dyn $ routeDyn <&> \case
        HomeR -> homePage
        CountriesMapR q -> countriesMapPage q
        CountriesListR q -> countriesListPage q
    footer_ $ p_ $ a_ [href_ "https://github.com/lagunoff"] "Vladislav Lagunov"

customCss :: Text
customCss = "\
  \body header, body main, body footer, body article {\
  \  max-width: 80rem;\
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
