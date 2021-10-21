module Pages where

import HtmlT
import Control.Lens
import Data.Generics.Labels ()
import Data.Foldable
import Data.Text as T
import Data.Ord
import Data.Maybe
import Data.JSString.Text
import GHCJS.Nullable
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List as L

import "this" Assets
import "this" Router
import "this" Utils

homePage :: Html ()
homePage = do
  h1_ "Home Page"

countriesListPage :: CountriesListQ -> Html ()
countriesListPage q@CountriesListQ{..} = div_ [class_ "CountriesList"] do
  queryRef <- newRef q
  form_ do
    onOptions "submit" (ListenerOpts True True True) $ const do
      pushUrl =<< readsRef (toUrl . CountriesListR . (set #page Nothing)) queryRef
    div_ [style_ "display:flex;"] do
      input_ [type_ "text", placeholder_ "Search countries by title", autofocus_ True ] do
        dynValue $ view (#search . to (fromMaybe "")) <$> fromRef queryRef
        onDecoder "input" valueDecoder \value -> modifyRef queryRef
          (set #search (Just value))
      button_ [type_ "submit"] "Search"
  table_ do
    thead_ $ tr_ do
      th_ ""
      thSort SortByTitle "Country Name"
      th_ "Region"
      th_ "Subregion"
      thSort SortByPopulation "Population"
    tbody_ do
      for_ pageResults \(n, Country{..}) -> tr_ do
        td_ do text (T.pack (show @Int n))
        td_ do
          a_ [href_ (toUrl (CountriesMapR CountriesMapQ {selected = Just (T.toLower code)}))] do
            for_ flag_icon (img_ . (>> style_ "display:inline"). src_)
            text title
        td_ do text region
        td_ do text subregion
        td_ do text (T.pack (show population))
  center_ do
    for_ (paginate total currentPage itemsPerPage) \case
      Nothing -> button_ [disabled_ True] "..."
      Just p -> a_
        [ href_ (toUrl (CountriesListR q {page = Just p}))] $
        button_ [disabled_ (currentPage == p)] $ text $ T.pack $ show p
  dl_ do
    dt_ "Country"
    dd_ $ unsafeHtml "The word <i>country</i> comes from <a href=\"\
      \https://en.wikipedia.org/wiki/Old_French\" title=\"Old French\">\
      \Old French</a> <i>contrée</i>, which derives from <a href=\"\
      \https://en.wikipedia.org/wiki/Vulgar_Latin\" title=\"Vulgar Latin\">\
      \Vulgar Latin</a> (<i>terra</i>) <i>contrata</i> (\"(land) lying \
      \opposite\"; \"(land) spread before\"), derived from <i>contra</i> \
      \(\"against, opposite\"). It most likely entered the English language \
      \after the <a href=\"https://en.wikipedia.org/wiki/Norman_invasion_of_\
      \England\" title=\"Norman invasion of England\">Franco-Norman invasion</a>\
      \ during the 11th century."
  where
    thSort sortBy title = th_ [style_ "cursor: pointer"] do
      text title
      case (fromMaybe SortByTitle sort_by, sort_dir) of
        (sortVal, Asc) | sortVal == sortBy -> text "▲"
        (sortVal, Desc) | sortVal == sortBy -> text "▼"
        otherwise -> text ""
      on_ "click" do pushUrl $ toUrl . CountriesListR . toggleSortBy sortBy $ q

    toggleSortBy sortBy q@CountriesListQ{..}
      | Just sb <- sort_by, sb == sortBy = q {sort_dir = flipDir sort_dir}
      | otherwise = q {sort_by = Just sortBy, sort_dir = Asc}
      where
        flipDir = \case Asc -> Desc; Desc -> Asc

    offset = maybe 0 pred page * itemsPerPage
    total = Prelude.length countryResults
    pageResults = Prelude.zip [offset + 1..]
      . Prelude.take itemsPerPage
      . Prelude.drop offset
      $ countryResults
    countryResults = L.sortOn countrySortDir
      . Prelude.filter countryFilter
      $ countries
    countryFilter
      | Just needle <- search = \Country{..} ->
        T.isInfixOf (T.toLower needle) (T.toLower title)
      | otherwise = const True
    countrySortBy = case sort_by of
      Just SortByPopulation -> \Country{..} -> Left population
      _ -> \Country{..} -> Right title
    countrySortDir = case sort_dir of
      Asc -> Left . countrySortBy
      Desc -> Right . Down . countrySortBy
    itemsPerPage = 40
    currentPage = fromMaybe 1 page

countriesMapPage :: CountriesMapQ -> Html ()
countriesMapPage CountriesMapQ{..} = div_ [class_ "CountriesMap"] do
  figure_ do
    center_ do
      unsafeHtml countriesMap
      figcaption_ "political map of the planet Earth"
      centerEl <- asks html_current_root
      liftIO $ js_selectCountry centerEl $ maybeToNullable $
        textToJSString <$> selected
      on "click" \event -> do
        mcode <- fmap textFromJSString . nullableToMaybe <$>
          liftIO (js_svgClickGetCountryCode event)
        mapM_ (pushUrl . toUrl . CountriesMapR . CountriesMapQ . Just) mcode

paginate
  :: Int -- ^ Total number of items
  -> Int -- ^ Current page
  -> Int -- ^ Items per page
  -> [Maybe Int] -- ^ List of page links, Nothing stands for ellipsis
paginate totalItems curPage limit
  | totalPages <= maxLinks =
    fmap Just [1..totalPages]
  | curPage <= 7 =
    fmap Just [1..8] <> [Nothing, Just totalPages]
  | curPage >= totalPages - 6 =
    [Just 1, Nothing] <> fmap Just [(totalPages - 8)..totalPages]
  | otherwise =
    [Just 1, Nothing] <> fmap Just [(curPage - 2)..(curPage + 3)]
    <> [Nothing, Just totalPages]
  where
    (pageQuot, pageRem) = totalItems `divMod` limit
    totalPages = if pageRem == 0 then pageQuot else pageQuot + 1
    maxLinks = 10
