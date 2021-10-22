module Router where

import Control.Lens
import Control.Monad
import Data.List as L
import Data.Maybe
import Data.Text as T
import GHC.Generics
import Network.URI
import Web.HttpApiData

data UrlParts = Url
  { partsPath :: [Text] -- ^ Path segments
  , partsQuery :: [(Text, Text)] -- ^ GET parameters
  } deriving (Eq, Show, Generic)

data Route
  = HomeR
  | CountriesMapR CountriesMapQ
  | CountriesListR CountriesListQ
  deriving (Eq, Show, Generic)

data CountriesListQ = CountriesListQ
  { search :: Maybe Text
  , page :: Int
  , sort_by :: CountrySortBy
  , sort_dir :: SortDir
  } deriving (Eq, Show, Generic)

data CountriesMapQ = CountriesMapQ
  { selected :: Maybe Text
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
    | selected <- L.lookup "selected" q
    -> Just $ CountriesMapR CountriesMapQ{..}
  Url ["list"] q
    | search <- L.lookup "search" q
    , page <- parsePage $ L.lookup "page" q
    , sort_dir <- parseSortDir $ L.lookup "sort_dir" q
    , sort_by <- parseSortBy $ L.lookup "sort_by" q
    -> Just $ CountriesListR CountriesListQ{..}
  _ -> Nothing
  where
    parsePage = fromMaybe (page defaultCountriesListQ)
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
      mfilter (/=page defaultCountriesListQ) . Just
    printSortDir = fmap (\case
      Asc -> "asc"
      Desc -> "desc") .
      mfilter (/=sort_dir defaultCountriesListQ) . Just
    printSortBy = fmap (\case
      SortByTitle -> "title"
      SortByPopulation -> "population"
      SortByRegion -> "region"
      SortBySubregion -> "subregion") .
      mfilter (/=sort_by defaultCountriesListQ) . Just

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

toUrl :: Route -> Text
toUrl = ("#"<>) . partsToText . printRoute

fromUrl :: Text -> Maybe Route
fromUrl url =
  parseRoute . textToParts . fromMaybe url . T.stripPrefix "#" $ url

partsToText :: UrlParts -> Text
partsToText (Url s q) = T.intercalate "?" (segments : query) where
  segments = T.intercalate "/" $ fmap escapeUri s
  query = L.filter (/="") . (:[]) . T.intercalate "&" . L.filter (/="")
    . fmap (\(k, v) -> k <> "=" <> v) $ fmap (bimap escapeUri escapeUri) q
  escapeUri = T.pack . escapeURIString isAllowed . T.unpack where
    isAllowed c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~:/,")

textToParts :: Text -> UrlParts
textToParts t = Url segments query where
  (segmentsText, queryText) = breakOn1 "?" t
  segments = fmap unEscapeUri . L.filter (/="") . T.splitOn "/" $
    segmentsText
  query = fmap (breakOn1 "=" . unEscapeUri) . L.filter (/="") . T.splitOn "&" $
    queryText
  breakOn1 s t = let (a, b) = T.breakOn s t in (a, T.drop 1 b)
  unEscapeUri = T.pack . unEscapeString . T.unpack
