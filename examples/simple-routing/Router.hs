module Router where

import Control.Monad
import Data.Bifunctor
import Data.List qualified as List
import Data.Maybe
import Data.Function
import GHC.Generics
import JavaScript.Compat.String (JSString(..))
import JavaScript.Compat.String qualified as JSS

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
    -> Just $ CountriesMapR CountriesMapQ{..}
  Url ["list"] q
    | search <- List.lookup "search" q
    , page <- parsePage $ List.lookup "page" q
    , sort_dir <- parseSortDir $ List.lookup "sort_dir" q
    , sort_by <- parseSortBy $ List.lookup "sort_by" q
    -> Just $ CountriesListR CountriesListQ{..}
  _ -> Nothing
  where
    parsePage = fromMaybe defaultCountriesListQ.page
      . (parseQueryParamMaybe =<<)
    parseSortDir = \case
      Just "asc" -> Asc
      Just "desc" -> Desc
      _ -> sort_dir defaultCountriesListQ
    parseSortBy = \case
      Just "title" -> SortByTitle
      Just "population" -> SortByPopulation
      Just "region" -> SortByRegion
      Just "subregion" -> SortBySubregion
      _ -> sort_by defaultCountriesListQ

printRoute :: Route -> UrlParts
printRoute = \case
  HomeR -> Url [] []
  CountriesMapR CountriesMapQ{..} -> Url ["map"] $ catMaybes
    [ ("selected",) <$> selected ]
  CountriesListR CountriesListQ{..} -> Url ["list"] $ catMaybes
    [ ("search",) <$> mfilter (/="") search
    , ("page",) <$> printPage page
    , ("sort_dir",) <$> printSortDir sort_dir
    , ("sort_by",) <$> printSortBy sort_by
    ]
  where
    printPage = fmap toQueryParam .
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

parseQueryParamMaybe = undefined
toQueryParam = undefined
