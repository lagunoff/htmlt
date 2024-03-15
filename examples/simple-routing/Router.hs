module Router where

import Control.Monad
import Data.Bifunctor
import Data.List qualified as List
import Data.Maybe
import Data.Function
import GHC.Generics
import Text.Read
import Data.Text (Text)
import Data.Text qualified as Text
import Wasm.Compat.Prim
import Wasm.Compat.Marshal
import Data.Char qualified as C

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
    parseIntQuery = readMaybe . Text.unpack

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
    toIntQuery = Text.pack . show

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
toUrl = ("#"<>) . printUrlParts . printRoute

fromUrl :: Text -> Maybe Route
fromUrl url = url
  & Text.stripPrefix "#"
  & fromMaybe url
  & parseUrlParts
  & parseRoute

printUrlParts :: UrlParts -> Text
printUrlParts (Url s q) = Text.intercalate "?" (segments : query)
  where
    segments =
      Text.intercalate "/" $ fmap encodeURIComponent s
    query = q
      & fmap (bimap encodeURIComponent encodeURIComponent)
      & fmap (\(k, v) -> k <> "=" <> v)
      & List.filter (not . Text.null)
      & Text.intercalate "&"
      & List.filter (not . Text.null) . (:[])

parseUrlParts :: Text -> UrlParts
parseUrlParts t = Url segments query
  where
    (segmentsStr, queryStr) = breakOn1 "?" t
    segments = segmentsStr
      & Text.splitOn "/"
      & List.filter (not . Text.null)
      & fmap decodeURIComponent
    query = queryStr
      & Text.splitOn "&"
      & List.filter (not . Text.null)
      & fmap (breakOn1 "=" . decodeURIComponent)
    breakOn1 s t =
      let (a, b) = Text.breakOn s t in (a, Text.drop 1 b)

encodeURIComponent :: Text -> Text
encodeURIComponent =
  Text.pack . concatMap encodeChar . Text.unpack
  where
    encodeChar c
      | C.isAlphaNum c = [c]
      | c == ' ' = "+"
      | otherwise = '%' : showHex (C.ord c) ""
    showHex :: Int -> String -> String
    showHex n acc
      | n < 16 = intToDigit n : acc
      | otherwise = let (q,r) = n `divMod` 16 in showHex q (intToDigit r : acc)
    intToDigit :: Int -> Char
    intToDigit n
      | 0 <= n && n <= 9 = toEnum (fromEnum '0' + n)
      | 10 <= n && n <= 15 = toEnum (fromEnum 'a' + n - 10)
      | otherwise = error "intToDigit: not a digit"

decodeURIComponent :: Text -> Text
decodeURIComponent =
  Text.pack . decode . Text.unpack
  where
    decode [] = []
    decode ('%':x1:x2:xs)
      | C.isHexDigit x1 && C.isHexDigit x2 =
        C.chr (16 * digitToInt x1 + digitToInt x2) : decode xs
    decode ('+':xs) = ' ' : decode xs
    decode (x:xs) = x : decode xs
    digitToInt :: Char -> Int
    digitToInt c
      | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
      | 'a' <= c && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | 'A' <= c && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = error "digitToInt: not a digit"
