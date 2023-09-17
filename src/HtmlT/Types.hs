{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Bool
import Data.Coerce
import Data.Maybe
import GHC.Generics
import HtmlT.Event
import Control.Monad.Fix
import GHC.JS.Prim
import GHC.Exts as Exts
import Data.String
import Unsafe.Coerce
import System.IO.Unsafe

-- | HtmlT is nothing more than just a newtype over ReaderT HtmlEnv
newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask, MonadTrans)

data HtmlEnv = HtmlEnv
  { html_current_element :: DOMElement
  -- ^ A DOMElement that will be used as a parent to insert new
  -- content, attributes, properties, listeners etc.
  , html_content_boundary :: Maybe ContentBoundary
  -- ^ Boundary defined by parent scope where new content should be
  -- attached, when Nothing whole parent element is available
  , html_reactive_env :: ReactiveEnv
  -- ^ Needed to implement 'HasReactiveEnv'
  } deriving Generic

-- | Most applications will only need HtmlT IO, hence this shortcut
type Html = HtmlT IO

-- | A newtype over JSVal which is an instance of Node
-- https://developer.mozilla.org/en-US/docs/Web/API/Node
newtype DOMNode = DOMNode {unDOMNode :: JSVal}

-- | A newtype over JSVal which is an instance of HTMLElement
-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
newtype DOMElement = DOMElement {unDOMElement :: JSVal}

-- | A newtype over JSVal which is an instance of Event
-- https://developer.mozilla.org/en-US/docs/Web/API/Event
newtype DOMEvent = DOMEvent {unDOMEvent :: JSVal}

-- | Untyped for simplicity and because it was annoying to not find
-- some of the new Events in ghcjs-dom where these names are
-- representated by data constructors
type EventName = JSString

newtype JSString = JSString {unJSString :: JSVal}

newtype Nullable v = Nullable {unNullable :: JSVal}

-- | Two 'DOMNode's (comments) that define boundaries for another
-- DOM content
data ContentBoundary = ContentBoundary
  { boundary_begin :: DOMNode
  , boundary_end :: DOMNode
  } deriving Generic

-- | Each DOMElement is also a valid DOMNode
nodeFromElement :: DOMElement -> DOMNode
nodeFromElement = coerce

runHtmlT :: HtmlT m a -> HtmlEnv -> m a
runHtmlT h = runReaderT (unHtmlT h)

execHtmlT :: HtmlEnv -> HtmlT m a -> m a
execHtmlT = flip runHtmlT

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Monad m => HasReactiveEnv (HtmlT m) where
  askReactiveEnv = asks html_reactive_env

maybeToNullable :: Coercible v JSVal => Maybe v -> Nullable v
maybeToNullable = \case
  Nothing -> Nullable jsNull -- FIXME: jsNull generates invalid javascript!
  Just v -> Nullable $ coerce v

class FromJSVal v where fromJSVal :: JSVal -> IO (Maybe v)

instance FromJSVal Int where
  fromJSVal = pure . Just . fromJSInt

instance FromJSVal Bool where
  fromJSVal = pure . Just . unsafeCoerce . js_bool (unsafeCoerce False) (unsafeCoerce True)

instance FromJSVal JSVal where
  fromJSVal = pure . Just

instance FromJSVal v => FromJSVal (Maybe v) where
  fromJSVal j = maybe (pure (Just Nothing)) fromJSVal $ nullableToMaybe (Nullable j)

instance FromJSVal v => FromJSVal [v] where
  fromJSVal s = fmap (Just . catMaybes) . mapM fromJSVal =<< fromJSArray s

class ToJSVal v where toJSVal :: v -> IO JSVal

instance ToJSVal Int where
  toJSVal = pure . toJSInt

instance ToJSVal Bool where
  toJSVal = pure . bool js_false js_true

instance ToJSVal JSVal where
  toJSVal = pure

instance ToJSVal v => ToJSVal (Maybe v) where
  toJSVal s = pure . unNullable . nullableFromMaybe =<< mapM toJSVal s

instance ToJSVal v => ToJSVal [v] where
  toJSVal s = toJSArray =<< mapM toJSVal s

deriving newtype instance FromJSVal JSString
deriving newtype instance ToJSVal JSString

#if !defined(javascript_HOST_ARCH)
js_stringEq :: JSVal -> JSVal -> Exts.Any -> Exts.Any -> Exts.Any
js_stringEq = undefined
js_bool :: Exts.Any -> Exts.Any -> JSVal -> Exts.Any
js_bool = undefined
js_true :: JSVal
js_true = undefined
js_false :: JSVal
js_false = undefined
#else
foreign import javascript unsafe
  "(($1, $2, ifTrue, ifFalse) => $1 == $2 ? ifTrue : ifFalse)"
  js_stringEq :: JSVal -> JSVal -> Exts.Any -> Exts.Any -> Exts.Any
foreign import javascript unsafe
  "((ifFalse, ifTrue, jsBool) => jsBool ? ifTrue : ifFalse)"
  js_bool :: Exts.Any -> Exts.Any -> JSVal -> Exts.Any
foreign import javascript unsafe
  "(() => true)" js_true :: JSVal
foreign import javascript unsafe
  "(() => false)" js_false :: JSVal
#endif

nullableToMaybe :: Nullable JSVal -> Maybe JSVal
nullableToMaybe (Nullable jsval)
  | isNull jsval = Nothing
  | otherwise    = Just jsval

nullableFromMaybe :: Maybe JSVal -> Nullable JSVal
nullableFromMaybe = Nullable . fromMaybe jsNull

fromHSString :: String -> JSString
fromHSString = JSString . toJSString

toHSString :: JSString -> String
toHSString = fromJSString . unJSString

instance Show JSString where
  show = show . toHSString
  {-# NOINLINE show #-}

instance Eq JSString where
  (==) (JSString a) (JSString b) = unsafeCoerce $ js_stringEq a b (unsafeCoerce True) (unsafeCoerce False)
  {-# NOINLINE (==) #-}

instance IsString JSString where
  fromString = JSString . toJSString
  {-# NOINLINE fromString #-}
