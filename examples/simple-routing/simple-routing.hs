import Control.Lens
import Control.Monad
import Data.Maybe
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
    header_ do
      h1_ "Simple in-browser routing example"
      p_ do
        "See the source on the "
        a_ [href_ "https://github.com/lagunoff/htmlt/blob/master/examples/simple-routing/"]
          "github"
      nav_ $ ul_ do
        let link t r = li_ $ a_ [href_ (toUrl r)] $ text t
        link "Home" HomeR
        link "Countries on the Map" PoliticalMapR
        link "List of Countries" $ CountriesR defaultCountriesQuery
    main_ do
      dyn $ routeDyn <&> \case
        HomeR -> homeWidget
        PoliticalMapR -> politicalMapWidget
        CountriesR q -> countriesWidget q
