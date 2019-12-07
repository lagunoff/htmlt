module Massaraksh.Html.Attribute.Dynamic where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Functor (void)
import Language.Javascript.JSaddle
import Massaraksh.Base (UI(..), UIHandle(..))
import Massaraksh.Event (Event(..))
import Massaraksh.Html.Base (Attr(..), Html, el)
import Massaraksh.Dynamic (Update(..), Dynamic(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

prop :: ToJSVal val => Text -> (i -> val) -> Attr msg i o
prop name f = Attr \model _ el -> do
  latestModel <- _dynRead model
  el <# name $ f latestModel
  unsubscribe <- _dynUpdates model `_evSubscribe` \Update{..} -> do
    void $ el <# name $ f _updNew
  pure unsubscribe

attr :: Text -> (i -> Text) -> Attr msg i o
attr name f = Attr \model _ el -> do
  latestModel <- _dynRead model
  el # "setAttribute" $ (name, f latestModel)
  unsubscribe <- _dynUpdates model `_evSubscribe` \Update{..} -> do
    void $ el # "setAttribute" $ (name, f _updNew)
  pure unsubscribe

boolProp :: Text -> (i -> Bool) -> Attr msg i o
boolProp = prop

stringProp :: Text -> (i -> String) -> Attr msg i o
stringProp = prop

textProp :: Text -> (i -> Text) -> Attr msg i o
textProp = prop

intProp :: Text -> (i -> Int) -> Attr msg i o
intProp = prop

doubleProp ::  Text -> (i -> Double) -> Attr msg i o
doubleProp = prop

unsafeHtml :: Text -> [Attr msg i o] -> (i -> Text) -> Html msg i o
unsafeHtml name other html =
  el name (prop (T.pack "innerHTML") html:other) []

text :: (i -> Text) -> Html msg i o
text f = UI \model _ -> do
  latestModel <- _dynRead model
  el <- jsg "document" # "createTextNode" $ f latestModel
  unsubscribe <- _dynUpdates model `_evSubscribe` \Update{..} ->
    void $ el <# "nodeValue" $ f _updNew
  pure (UIHandle el unsubscribe)

lazyText :: (i -> LT.Text) -> Html msg i o
lazyText f = UI \model _ -> do
  latestModel <- _dynRead model
  el <- jsg "document" # "createTextNode" $ LT.toStrict (f latestModel)
  unsubscribe <- _dynUpdates model `_evSubscribe` \Update{..} ->
    void $ el <# "nodeValue" $ LT.toStrict (f _updNew)
  pure (UIHandle el unsubscribe)

-- |Borrowed from @Miso.Html.Property@
-- https://github.com/dmjio/miso/blob/f99ccad2ef7c1d8d56ad848ac7284f35a8b2b19c/src/Miso/Html/Property.hs#L147

-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null . _items) ] [ ]
classList_ :: [(Text, i -> Bool)] -> Attr msg i o
classList_ classes = class_ \i -> T.unwords $ foldl' (\acc (cs, f) -> if f i then cs:acc else acc) [] classes
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/style>
style_ :: (i -> Text) -> Attr msg i o
style_ = textProp $ T.pack  "style"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/title>
title_ ::  (i -> Text) -> Attr msg i o
title_ = textProp $ T.pack "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/selected>
selected_ ::  (i -> Bool) -> Attr msg i o
selected_ = boolProp $ T.pack "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/hidden>
hidden_ ::  (i -> Bool) -> Attr msg i o
hidden_             = boolProp $ T.pack "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/value>
value_ ::  (i -> Text) -> Attr msg i o
value_             = textProp $ T.pack "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/defaultValue>
defaultValue_ ::  (i -> Text) -> Attr msg i o
defaultValue_      = textProp $ T.pack "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/accept>
accept_ ::  (i -> Text) -> Attr msg i o
accept_            = textProp $ T.pack "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/acceptCharset>
acceptCharset_ ::  (i -> Text) -> Attr msg i o
acceptCharset_     = textProp $ T.pack "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/action>
action_ ::  (i -> Text) -> Attr msg i o
action_            = textProp $ T.pack "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autocomplete>
autocomplete_ ::  (i -> Bool) -> Attr msg i o
autocomplete_ b = textProp (T.pack "autocomplete") (\i -> T.pack $ if b i then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autosave>
autosave_ ::  (i -> Text) -> Attr msg i o
autosave_          = textProp $ T.pack "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/disabled>
disabled_ ::  (i -> Bool) -> Attr msg i o
disabled_          = boolProp $ T.pack "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/enctype>
enctype_ ::  (i -> Text) -> Attr msg i o
enctype_           = textProp $ T.pack "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/formation>
formation_ ::  (i -> Text) -> Attr msg i o
formation_         = textProp $ T.pack "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/list>
list_ ::  (i -> Text) -> Attr msg i o
list_              = textProp $ T.pack "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/maxlength>
maxlength_ ::  (i -> Text) -> Attr msg i o
maxlength_         = textProp $ T.pack "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/minlength>
minlength_ ::  (i -> Text) -> Attr msg i o
minlength_         = textProp $ T.pack "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/method>
method_ ::  (i -> Text) -> Attr msg i o
method_            = textProp $ T.pack "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/multiple>
multiple_ ::  (i -> Bool) -> Attr msg i o
multiple_          = boolProp $ T.pack "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/novalidate>
novalidate_ ::  (i -> Bool) -> Attr msg i o
novalidate_        = boolProp $ T.pack "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/pattern>
pattern_ ::  (i -> Text) -> Attr msg i o
pattern_           = textProp $ T.pack "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/readonly>
readonly_ ::  (i -> Bool) -> Attr msg i o
readonly_          = boolProp $ T.pack "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/required>
required_ ::  (i -> Bool) -> Attr msg i o
required_          = boolProp $ T.pack "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/size>
size_ ::  (i -> Text) -> Attr msg i o
size_              = textProp $ T.pack "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/for>
htmlFor_ ::  (i -> Text) -> Attr msg i o
htmlFor_               = textProp $ T.pack "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/form>
form_ ::  (i -> Text) -> Attr msg i o
form_               = textProp $ T.pack "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/max>
max_ ::  (i -> Text) -> Attr msg i o
max_               = textProp $ T.pack "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/min>
min_ ::  (i -> Text) -> Attr msg i o
min_               = textProp $ T.pack "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/step>
step_ ::  (i -> Text) -> Attr msg i o
step_              = textProp $ T.pack "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/cols>
cols_ ::  (i -> Text) -> Attr msg i o
cols_              = textProp $ T.pack "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/rows>
rows_ ::  (i -> Text) -> Attr msg i o
rows_              = textProp $ T.pack "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/wrap>
wrap_ ::  (i -> Text) -> Attr msg i o
wrap_              = textProp $ T.pack "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/target>
target_ ::  (i -> Text) -> Attr msg i o
target_            = textProp $ T.pack "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/download>
download_ ::  (i -> Text) -> Attr msg i o
download_          = textProp $ T.pack "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/downloadAs>
downloadAs_ ::  (i -> Text) -> Attr msg i o
downloadAs_        = textProp $ T.pack "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/hreflang>
hreflang_ ::  (i -> Text) -> Attr msg i o
hreflang_          = textProp $ T.pack "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/media>
media_ ::  (i -> Text) -> Attr msg i o
media_             = textProp $ T.pack "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/ping>
ping_ ::  (i -> Text) -> Attr msg i o
ping_              = textProp $ T.pack "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/rel>
rel_ ::  (i -> Text) -> Attr msg i o
rel_               = textProp $ T.pack "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/ismap>
ismap_ ::  (i -> Text) -> Attr msg i o
ismap_             = textProp $ T.pack "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/usemap>
usemap_ ::  (i -> Text) -> Attr msg i o
usemap_            = textProp $ T.pack "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/shape>
shape_ ::  (i -> Text) -> Attr msg i o
shape_             = textProp $ T.pack "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/coords>
coords_ ::  (i -> Text) -> Attr msg i o
coords_            = textProp $ T.pack "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/src>
src_ ::  (i -> Text) -> Attr msg i o
src_               = textProp $ T.pack "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/height>
height_ ::  (i -> Text) -> Attr msg i o
height_            = textProp $ T.pack "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/width>
width_ ::  (i -> Text) -> Attr msg i o
width_             = textProp $ T.pack "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/alt>
alt_ ::  (i -> Text) -> Attr msg i o
alt_               = textProp $ T.pack "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autoplay>
autoplay_ ::  (i -> Bool) -> Attr msg i o
autoplay_          = boolProp $ T.pack "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/controls>
controls_ ::  (i -> Bool) -> Attr msg i o
controls_          = boolProp $ T.pack "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/loop>
loop_ ::  (i -> Bool) -> Attr msg i o
loop_              = boolProp $ T.pack "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/preload>
preload_ ::  (i -> Text) -> Attr msg i o
preload_           = textProp $ T.pack "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/poster>
poster_ ::  (i -> Text) -> Attr msg i o
poster_            = textProp $ T.pack "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/default>
default_ ::  (i -> Bool) -> Attr msg i o
default_           = boolProp $ T.pack "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/kind>
kind_ ::  (i -> Text) -> Attr msg i o
kind_              = textProp $ T.pack "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/srclang>
srclang_ ::  (i -> Text) -> Attr msg i o
srclang_           = textProp $ T.pack "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/sandbox>
sandbox_ ::  (i -> Text) -> Attr msg i o
sandbox_           = textProp $ T.pack "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/seamless>
seamless_ ::  (i -> Text) -> Attr msg i o
seamless_          = textProp $ T.pack "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/srcdoc>
srcdoc_ ::  (i -> Text) -> Attr msg i o
srcdoc_            = textProp $ T.pack "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/reversed>
reversed_ ::  (i -> Text) -> Attr msg i o
reversed_          = textProp $ T.pack "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/start>
start_ ::  (i -> Text) -> Attr msg i o
start_             = textProp $ T.pack "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/align>
align_ ::  (i -> Text) -> Attr msg i o
align_             = textProp $ T.pack "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/colspan>
colspan_ ::  (i -> Text) -> Attr msg i o
colspan_           = textProp $ T.pack "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/rowspan>
rowspan_ ::  (i -> Text) -> Attr msg i o
rowspan_           = textProp $ T.pack "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/headers>
headers_ ::  (i -> Text) -> Attr msg i o
headers_           = textProp $ T.pack "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/scope>
scope_ ::  (i -> Text) -> Attr msg i o
scope_             = textProp $ T.pack "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/async>
async_ ::  (i -> Text) -> Attr msg i o
async_             = textProp $ T.pack "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/charset>
charset_ ::  (i -> Text) -> Attr msg i o
charset_           = textProp $ T.pack "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/content>
content_ ::  (i -> Text) -> Attr msg i o
content_           = textProp $ T.pack "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/defer>
defer_ ::  (i -> Text) -> Attr msg i o
defer_             = textProp $ T.pack "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/httpEquiv>
httpEquiv_ ::  (i -> Text) -> Attr msg i o
httpEquiv_         = textProp $ T.pack "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/language>
language_ ::  (i -> Text) -> Attr msg i o
language_          = textProp $ T.pack "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/scoped>
scoped_ ::  (i -> Text) -> Attr msg i o
scoped_            = textProp $ T.pack "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/type>
type_ ::  (i -> Text) -> Attr msg i o
type_ = textProp $ T.pack "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/name>
name_ ::  (i -> Text) -> Attr msg i o
name_ = textProp $ T.pack "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/href>
href_ ::  (i -> Text) -> Attr msg i o
href_ = textProp $ T.pack "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/id>
id_ ::  (i -> Text) -> Attr msg i o
id_ = textProp $ T.pack "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/placeholder>
placeholder_ ::  (i -> Text) -> Attr msg i o
placeholder_ = textProp $ T.pack "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/checked>
checked_ ::  (i -> Bool) -> Attr msg i o
checked_ = boolProp $ T.pack "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autofocus>
autofocus_ ::  (i -> Bool) -> Attr msg i o
autofocus_ = boolProp $ T.pack "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  (i -> Text) -> Attr msg i o
class_ = textProp $ T.pack "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  Text -> (i -> Text) -> Attr msg i o
data_ k v = textProp (T.pack "data-" <> k) v

unsafeInnerHTML :: (i -> Text) -> Attr msg i o
unsafeInnerHTML = textProp $ T.pack "innerHTML"
