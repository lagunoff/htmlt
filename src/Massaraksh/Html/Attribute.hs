{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Property@
-- https://github.com/dmjio/miso/blob/f99ccad2ef7c1d8d56ad848ac7284f35a8b2b19c/src/Miso/Html/Property.hs#L147
module Massaraksh.Html.Attribute where

import Data.Text (Text)
import qualified Data.Text as T
import Massaraksh.Html.Base
import Data.Foldable (foldl')


-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null . _items) ] [ ]
classList_ :: [(Text, Bool)] -> Attr msg i o
classList_ classes = class_ $ T.unwords $ foldl' (\acc (cs, cond) -> if cond then cs:acc else acc) [] classes
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/style>
style_ ::  Text -> Attr msg i o
style_ = textProp "style"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/title>
title_ ::  Text -> Attr msg i o
title_ = textProp "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/selected>
selected_ ::  Bool -> Attr msg i o
selected_ = boolProp "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/hidden>
hidden_ ::  Bool -> Attr msg i o
hidden_             = boolProp "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/value>
value_ ::  Text -> Attr msg i o
value_             = textProp "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/defaultValue>
defaultValue_ ::  Text -> Attr msg i o
defaultValue_      = textProp "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/accept>
accept_ ::  Text -> Attr msg i o
accept_            = textProp "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/acceptCharset>
acceptCharset_ ::  Text -> Attr msg i o
acceptCharset_     = textProp "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/action>
action_ ::  Text -> Attr msg i o
action_            = textProp "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autocomplete>
autocomplete_ ::  Bool -> Attr msg i o
autocomplete_ b = textProp "autocomplete" (if b then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autosave>
autosave_ ::  Text -> Attr msg i o
autosave_          = textProp "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/disabled>
disabled_ ::  Bool -> Attr msg i o
disabled_          = boolProp "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/enctype>
enctype_ ::  Text -> Attr msg i o
enctype_           = textProp "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/formation>
formation_ ::  Text -> Attr msg i o
formation_         = textProp "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/list>
list_ ::  Text -> Attr msg i o
list_              = textProp "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/maxlength>
maxlength_ ::  Text -> Attr msg i o
maxlength_         = textProp "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/minlength>
minlength_ ::  Text -> Attr msg i o
minlength_         = textProp "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/method>
method_ ::  Text -> Attr msg i o
method_            = textProp "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/multiple>
multiple_ ::  Bool -> Attr msg i o
multiple_          = boolProp "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/novalidate>
novalidate_ ::  Bool -> Attr msg i o
novalidate_        = boolProp "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/pattern>
pattern_ ::  Text -> Attr msg i o
pattern_           = textProp "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/readonly>
readonly_ ::  Bool -> Attr msg i o
readonly_          = boolProp "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/required>
required_ ::  Bool -> Attr msg i o
required_          = boolProp "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/size>
size_ ::  Text -> Attr msg i o
size_              = textProp "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/for>
htmlFor_ ::  Text -> Attr msg i o
htmlFor_               = attr "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/form>
form_ ::  Text -> Attr msg i o
form_               = textProp "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/max>
max_ ::  Text -> Attr msg i o
max_               = textProp "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/min>
min_ ::  Text -> Attr msg i o
min_               = textProp "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/step>
step_ ::  Text -> Attr msg i o
step_              = textProp "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/cols>
cols_ ::  Text -> Attr msg i o
cols_              = textProp "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/rows>
rows_ ::  Text -> Attr msg i o
rows_              = textProp "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/wrap>
wrap_ ::  Text -> Attr msg i o
wrap_              = textProp "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/target>
target_ ::  Text -> Attr msg i o
target_            = textProp "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/download>
download_ ::  Text -> Attr msg i o
download_          = textProp "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/downloadAs>
downloadAs_ ::  Text -> Attr msg i o
downloadAs_        = textProp "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/hreflang>
hreflang_ ::  Text -> Attr msg i o
hreflang_          = textProp "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/media>
media_ ::  Text -> Attr msg i o
media_             = textProp "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/ping>
ping_ ::  Text -> Attr msg i o
ping_              = textProp "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/rel>
rel_ ::  Text -> Attr msg i o
rel_               = textProp "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/ismap>
ismap_ ::  Text -> Attr msg i o
ismap_             = textProp "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/usemap>
usemap_ ::  Text -> Attr msg i o
usemap_            = textProp "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/shape>
shape_ ::  Text -> Attr msg i o
shape_             = textProp "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/coords>
coords_ ::  Text -> Attr msg i o
coords_            = textProp "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/src>
src_ ::  Text -> Attr msg i o
src_               = textProp "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/height>
height_ ::  Text -> Attr msg i o
height_            = textProp "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/width>
width_ ::  Text -> Attr msg i o
width_             = textProp "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/alt>
alt_ ::  Text -> Attr msg i o
alt_               = textProp "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autoplay>
autoplay_ ::  Bool -> Attr msg i o
autoplay_          = boolProp "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/controls>
controls_ ::  Bool -> Attr msg i o
controls_          = boolProp "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/loop>
loop_ ::  Bool -> Attr msg i o
loop_              = boolProp "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/preload>
preload_ ::  Text -> Attr msg i o
preload_           = textProp "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/poster>
poster_ ::  Text -> Attr msg i o
poster_            = textProp "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/default>
default_ ::  Bool -> Attr msg i o
default_           = boolProp "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/kind>
kind_ ::  Text -> Attr msg i o
kind_              = textProp "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/srclang>
srclang_ ::  Text -> Attr msg i o
srclang_           = textProp "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/sandbox>
sandbox_ ::  Text -> Attr msg i o
sandbox_           = textProp "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/seamless>
seamless_ ::  Text -> Attr msg i o
seamless_          = textProp "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/srcdoc>
srcdoc_ ::  Text -> Attr msg i o
srcdoc_            = textProp "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/reversed>
reversed_ ::  Text -> Attr msg i o
reversed_          = textProp "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/start>
start_ ::  Text -> Attr msg i o
start_             = textProp "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/align>
align_ ::  Text -> Attr msg i o
align_             = textProp "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/colspan>
colspan_ ::  Text -> Attr msg i o
colspan_           = textProp "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/rowspan>
rowspan_ ::  Text -> Attr msg i o
rowspan_           = textProp "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/headers>
headers_ ::  Text -> Attr msg i o
headers_           = textProp "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/scope>
scope_ ::  Text -> Attr msg i o
scope_             = textProp "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/async>
async_ ::  Text -> Attr msg i o
async_             = textProp "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/charset>
charset_ ::  Text -> Attr msg i o
charset_           = textProp "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/content>
content_ ::  Text -> Attr msg i o
content_           = textProp "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/defer>
defer_ ::  Text -> Attr msg i o
defer_             = textProp "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/httpEquiv>
httpEquiv_ ::  Text -> Attr msg i o
httpEquiv_         = textProp "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/language>
language_ ::  Text -> Attr msg i o
language_          = textProp "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/scoped>
scoped_ ::  Text -> Attr msg i o
scoped_            = textProp "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/type>
type_ ::  Text -> Attr msg i o
type_ = textProp "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/name>
name_ ::  Text -> Attr msg i o
name_ = textProp "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/href>
href_ ::  Text -> Attr msg i o
href_ = textProp "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/id>
id_ ::  Text -> Attr msg i o
id_ = textProp "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/placeholder>
placeholder_ ::  Text -> Attr msg i o
placeholder_ = textProp "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/checked>
checked_ ::  Bool -> Attr msg i o
checked_ = boolProp "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attr/autofocus>
autofocus_ ::  Bool -> Attr msg i o
autofocus_ = boolProp "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  Text -> Attr msg i o
class_ = textProp "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  Text -> Text -> Attr msg i o
data_ k v = textProp ("data-" <> k) v

unsafeInnerHTML :: Text -> Attr msg i o
unsafeInnerHTML = textProp "innerHTML"
