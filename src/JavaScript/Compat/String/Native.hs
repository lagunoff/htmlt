module JavaScript.Compat.String.Native where

import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts as Exts
import JavaScript.Compat.Prim

newtype JSString = JSString {unJSString :: Text}
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid)

toJSValPure :: JSString -> JSVal
toJSValPure = undefined

fromJSValPure :: JSVal -> JSString
fromJSValPure = undefined

empty :: JSString
empty = JSString T.empty
{-# INLINE empty #-}

pack :: String -> JSString
pack = JSString . T.pack
{-# INLINE pack #-}

unpack :: JSString -> String
unpack = T.unpack . unJSString
{-# INLINE unpack #-}

strip :: JSString -> JSString
strip = JSString . T.strip . unJSString
{-# INLINE strip #-}

append :: JSString -> JSString -> JSString
append (JSString a) (JSString b) = JSString $ T.append a b
{-# INLINE append #-}

stripPrefix :: JSString -> JSString -> Maybe JSString
stripPrefix (JSString a) (JSString b) =
  JSString <$> T.stripPrefix a b
{-# INLINE stripPrefix #-}

breakOn :: JSString -> JSString -> (JSString, JSString)
breakOn (JSString a) (JSString b) =
  let (c, d) = T.breakOn a b in (JSString c, JSString d)
{-# INLINE breakOn #-}

splitOn :: JSString -> JSString -> [JSString]
splitOn (JSString a) (JSString b) = fmap JSString $ T.splitOn a b
{-# NOINLINE splitOn #-}

intercalate :: JSString -> [JSString] -> JSString
intercalate (JSString s) l = JSString $ T.intercalate s $ fmap unJSString l
{-# INLINE intercalate #-}

drop :: Int -> JSString -> JSString
drop n (JSString s) = JSString $ T.drop n s
{-# INLINE drop #-}

take :: Int -> JSString -> JSString
take n (JSString s) = JSString $ T.take n s
{-# INLINE take #-}

encodeURIComponent :: JSString -> JSString
encodeURIComponent =
  JSString . T.pack . concatMap encodeChar . T.unpack . unJSString
  where
    encodeChar c
      | C.isAlphaNum c = [c]
      | c == ' ' = "+"
      | otherwise = '%' : showHex (C.ord c) ""

decodeURIComponent :: JSString -> JSString
decodeURIComponent = JSString . T.pack . decode . T.unpack . unJSString
  where
    decode [] = []
    decode ('%':x1:x2:xs)
      | C.isHexDigit x1 && C.isHexDigit x2 =
        C.chr (16 * digitToInt x1 + digitToInt x2) : decode xs
    decode ('+':xs) = ' ' : decode xs
    decode (x:xs) = x : decode xs

showHex :: Int -> String -> String
showHex n acc
  | n < 16 = intToDigit n : acc
  | otherwise = let (q,r) = n `divMod` 16 in showHex q (intToDigit r : acc)

digitToInt :: Char -> Int
digitToInt c
  | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
  | 'a' <= c && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | 'A' <= c && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise = error "digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit n
  | 0 <= n && n <= 9 = toEnum (fromEnum '0' + n)
  | 10 <= n && n <= 15 = toEnum (fromEnum 'a' + n - 10)
  | otherwise = error "intToDigit: not a digit"

toLower :: JSString -> JSString
toLower = JSString . T.toLower . unJSString
{-# INLINE toLower #-}

toUpper :: JSString -> JSString
toUpper = JSString . T.toUpper . unJSString
{-# INLINE toUpper #-}

isInfixOf :: JSString -> JSString -> Bool
isInfixOf (JSString a) (JSString b) = T.isInfixOf a b
{-# INLINE isInfixOf #-}

null :: JSString -> Bool
null = T.null . unJSString
{-# INLINE null #-}
