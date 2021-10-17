module Router where

import Control.Monad
import Control.Lens
import Data.Maybe
import Data.List as L
import Data.Text as T
import Network.URI
import GHC.Generics
import Web.HttpApiData

data UrlParts = Url
  { partsPath :: [Text] -- ^ Path segments
  , partsQuery :: [(Text, Text)] -- ^ GET parameters
  } deriving (Eq, Show, Generic)

data Route
  = HomeR
  | PoliticalMapR
  | CountriesR CountriesQ
  deriving (Eq, Show, Generic)

toRoute :: UrlParts -> Maybe Route
toRoute = \case
  Url [] [] -> Just HomeR
  Url ["map"] _ -> Just PoliticalMapR
  Url ["countries"] q
    | search <- L.lookup "search" q
    , page <- parseQueryParamMaybe <=< L.lookup "page" $ q
    , sort_dir <- fromMaybe Asc $ parseSortDir <=< L.lookup "sort_dir" $ q
    , sort_by <- parseSortBy <=< L.lookup "sort_by" $ q
    -> Just $ CountriesR CountriesQ{..}
  _ -> Nothing
  where
    parseSortDir = \case
      "asc" -> Just Asc
      "desc" -> Just Desc
      _ -> Nothing
    parseSortBy = \case
      "title" -> Just SortByTitle
      "population" -> Just SortByPopulation
      _ -> Nothing

fromRoute :: Route -> UrlParts
fromRoute = \case
  HomeR -> Url [] []
  PoliticalMapR -> Url ["map"] []
  CountriesR CountriesQ{..} -> Url ["countries"] $ catMaybes
    [ ("search",) <$> mfilter (/="") search
    , ("page",) . toQueryParam <$> mfilter (/=0) page
    , ("sort_dir",) . printSortDir <$> mfilter (/=Asc) (Just sort_dir)
    , ("sort_by",) . printSortBy <$> sort_by
    ]
  where
    printSortDir = \case
      Asc -> "asc"
      Desc -> "desc"
    printSortBy = \case
      SortByTitle -> "title"
      SortByPopulation -> "population"

data CountriesQ = CountriesQ
  { search :: Maybe Text
  , page :: Maybe Int
  , sort_by :: Maybe CountrySortBy
  , sort_dir :: SortDir
  } deriving (Eq, Show, Generic)

data SortDir = Asc | Desc
  deriving (Eq, Show, Generic)

data CountrySortBy = SortByTitle | SortByPopulation
  deriving (Eq, Show, Generic)

defaultCountriesQuery :: CountriesQ
defaultCountriesQuery = CountriesQ
  { search = Nothing
  , page = Nothing
  , sort_by = Nothing
  , sort_dir = Asc
  }

toUrl :: Route -> Text
toUrl = ("#"<>) . partsToText . fromRoute

fromUrl :: Text -> Maybe Route
fromUrl url =
  toRoute . textToParts . fromMaybe url . T.stripPrefix "#" $ url

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
