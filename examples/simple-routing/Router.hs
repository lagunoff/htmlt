module Router where

import Control.Monad
import Data.Bifunctor
import Data.List qualified as List
import Data.Maybe
import Data.Function
import GHC.Generics
import Text.Read
import Wasm.Compat.Prim
import Wasm.Compat.Marshal

data UrlParts = Url
  { partsPath :: [JSString] -- ^ Path segments
  , partsQuery :: [(JSString, JSString)] -- ^ GET parameters
  } deriving (Eq, Show, Generic)

data Route
  = HomeR
  | CountriesMapR CountriesMapQ
  | CountriesListR CountriesListQ
  deriving (Eq, Show, Generic)

data CountriesListQ = CountriesListQ
  { search :: Maybe JSString
  , page :: Int
  , sort_by :: CountrySortBy
  , sort_dir :: SortDir
  } deriving (Eq, Show, Generic)

data CountriesMapQ = CountriesMapQ
  { selected :: Maybe JSString
  } deriving (Eq, Show, Generic)

data SortDir = Asc | Desc
  deriving (Eq, Show, Generic)

data CountrySortBy
  = SortByTitle
  | SortByPopulation
  | SortByRegion
  | SortBySubregion
  deriving (Eq, Show, Generic)

parseRoute :: UrlParts -> Maybe Route
parseRoute = \case
  Url [] [] -> Just HomeR
  Url ["map"] q
    | selected <- List.lookup "selected" q
    -> Just $ CountriesMapR CountriesMapQ{selected}
  Url ["list"] q
    | search <- List.lookup "search" q
    , page <- parsePage $ List.lookup "page" q
    , sort_dir <- parseSortDir $ List.lookup "sort_dir" q
    , sort_by <- parseSortBy $ List.lookup "sort_by" q
    -> Just $ CountriesListR CountriesListQ{search, page, sort_dir, sort_by}
  _ -> Nothing
  where
    parsePage = fromMaybe defaultCountriesListQ.page
      . (parseIntQuery =<<)
    parseSortDir = \case
      Just "asc" -> Asc
      Just "desc" -> Desc
      _ -> defaultCountriesListQ.sort_dir
    parseSortBy = \case
      Just "title" -> SortByTitle
      Just "population" -> SortByPopulation
      Just "region" -> SortByRegion
      Just "subregion" -> SortBySubregion
      _ -> defaultCountriesListQ.sort_by
    parseIntQuery = readMaybe . fromJSString

printRoute :: Route -> UrlParts
printRoute = \case
  HomeR -> Url [] []
  CountriesMapR q -> Url ["map"] $ catMaybes
    [ ("selected",) <$> q.selected ]
  CountriesListR q -> Url ["list"] $ catMaybes
    [ ("search",) <$> mfilter (/="") q.search
    , ("page",) <$> printPage q.page
    , ("sort_dir",) <$> printSortDir q.sort_dir
    , ("sort_by",) <$> printSortBy q.sort_by
    ]
  where
    printPage = fmap toIntQuery .
      mfilter (/=defaultCountriesListQ.page) . Just
    printSortDir = fmap (\case
      Asc -> "asc"
      Desc -> "desc") .
      mfilter (/=defaultCountriesListQ.sort_dir) . Just
    printSortBy = fmap (\case
      SortByTitle -> "title"
      SortByPopulation -> "population"
      SortByRegion -> "region"
      SortBySubregion -> "subregion") .
      mfilter (/=defaultCountriesListQ.sort_by) . Just
    toIntQuery = JSS.pack . show

defaultCountriesListQ :: CountriesListQ
defaultCountriesListQ = CountriesListQ
  { search = Nothing
  , page = 1
  , sort_by = SortByPopulation
  , sort_dir = Desc
  }

defaultCountriesMapQ :: CountriesMapQ
defaultCountriesMapQ = CountriesMapQ
  { selected = Nothing
  }

toUrl :: Route -> JSString
toUrl = ("#"<>) . partsToText . printRoute

fromUrl :: JSString -> Maybe Route
fromUrl url = url
  & JSS.stripPrefix "#"
  & fromMaybe url
  & textToParts
  & parseRoute

partsToText :: UrlParts -> JSString
partsToText (Url s q) = JSS.intercalate "?" (segments : query)
  where
    segments =
      JSS.intercalate "/" $ fmap JSS.encodeURIComponent s
    query = q
      & fmap (bimap JSS.encodeURIComponent JSS.encodeURIComponent)
      & fmap (\(k, v) -> k <> "=" <> v)
      & List.filter (not . JSS.null)
      & JSS.intercalate "&"
      & List.filter (not . JSS.null) . (:[])

textToParts :: JSString -> UrlParts
textToParts t = Url segments query
  where
    (segmentsStr, queryStr) = breakOn1 "?" t
    segments = segmentsStr
      & JSS.splitOn "/"
      & List.filter (not . JSS.null)
      & fmap JSS.decodeURIComponent
    query = queryStr
      & JSS.splitOn "&"
      & List.filter (not . JSS.null)
      & fmap (breakOn1 "=" . JSS.decodeURIComponent)
    breakOn1 s t =
      let (a, b) = JSS.breakOn s t in (a, JSS.drop 1 b)
