{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Property@
-- https://github.com/dmjio/miso/blob/f99ccad2ef7c1d8d56ad848ac7284f35a8b2b19c/src/Miso/Html/Property.hs#L147
module Massaraksh.Html.Attribute.Dynamic where

import Data.Foldable (foldl')
import Data.Text (Text)
import Massaraksh.Html.Core
import qualified Data.Text as T
import qualified Massaraksh.Html.Core.Dynamic as Dyn

-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null . _items) ] [ ]
classList_ :: [(Text, i -> Bool)] -> Attribute msg i o
classList_ classes = class_ \i -> T.unwords $ foldl' (\acc (cs, f) -> if f i then cs:acc else acc) [] classes
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/style>
style_ :: (i -> Text) -> Attribute msg i o
style_ = Dyn.textProp "style"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/title>
title_ ::  (i -> Text) -> Attribute msg i o
title_ = Dyn.textProp "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/selected>
selected_ ::  (i -> Bool) -> Attribute msg i o
selected_ = Dyn.boolProp "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hidden>
hidden_ ::  (i -> Bool) -> Attribute msg i o
hidden_             = Dyn.boolProp "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/value>
value_ ::  (i -> Text) -> Attribute msg i o
value_             = Dyn.textProp "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defaultValue>
defaultValue_ ::  (i -> Text) -> Attribute msg i o
defaultValue_      = Dyn.textProp "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/accept>
accept_ ::  (i -> Text) -> Attribute msg i o
accept_            = Dyn.textProp "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/acceptCharset>
acceptCharset_ ::  (i -> Text) -> Attribute msg i o
acceptCharset_     = Dyn.textProp "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/action>
action_ ::  (i -> Text) -> Attribute msg i o
action_            = Dyn.textProp "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autocomplete>
autocomplete_ ::  (i -> Bool) -> Attribute msg i o
autocomplete_ b = Dyn.textProp "autocomplete" (\i -> if b i then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autosave>
autosave_ ::  (i -> Text) -> Attribute msg i o
autosave_          = Dyn.textProp "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/disabled>
disabled_ ::  (i -> Bool) -> Attribute msg i o
disabled_          = Dyn.boolProp "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/enctype>
enctype_ ::  (i -> Text) -> Attribute msg i o
enctype_           = Dyn.textProp "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/formation>
formation_ ::  (i -> Text) -> Attribute msg i o
formation_         = Dyn.textProp "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/list>
list_ ::  (i -> Text) -> Attribute msg i o
list_              = Dyn.textProp "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/maxlength>
maxlength_ ::  (i -> Text) -> Attribute msg i o
maxlength_         = Dyn.textProp "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/minlength>
minlength_ ::  (i -> Text) -> Attribute msg i o
minlength_         = Dyn.textProp "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/method>
method_ ::  (i -> Text) -> Attribute msg i o
method_            = Dyn.textProp "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/multiple>
multiple_ ::  (i -> Bool) -> Attribute msg i o
multiple_          = Dyn.boolProp "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/novalidate>
novalidate_ ::  (i -> Bool) -> Attribute msg i o
novalidate_        = Dyn.boolProp "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/pattern>
pattern_ ::  (i -> Text) -> Attribute msg i o
pattern_           = Dyn.textProp "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/readonly>
readonly_ ::  (i -> Bool) -> Attribute msg i o
readonly_          = Dyn.boolProp "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/required>
required_ ::  (i -> Bool) -> Attribute msg i o
required_          = Dyn.boolProp "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/size>
size_ ::  (i -> Text) -> Attribute msg i o
size_              = Dyn.textProp "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/for>
htmlFor_ ::  (i -> Text) -> Attribute msg i o
htmlFor_               = Dyn.textProp "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/form>
form_ ::  (i -> Text) -> Attribute msg i o
form_               = Dyn.textProp "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/max>
max_ ::  (i -> Text) -> Attribute msg i o
max_               = Dyn.textProp "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/min>
min_ ::  (i -> Text) -> Attribute msg i o
min_               = Dyn.textProp "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/step>
step_ ::  (i -> Text) -> Attribute msg i o
step_              = Dyn.textProp "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/cols>
cols_ ::  (i -> Text) -> Attribute msg i o
cols_              = Dyn.textProp "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rows>
rows_ ::  (i -> Text) -> Attribute msg i o
rows_              = Dyn.textProp "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/wrap>
wrap_ ::  (i -> Text) -> Attribute msg i o
wrap_              = Dyn.textProp "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/target>
target_ ::  (i -> Text) -> Attribute msg i o
target_            = Dyn.textProp "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/download>
download_ ::  (i -> Text) -> Attribute msg i o
download_          = Dyn.textProp "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/downloadAs>
downloadAs_ ::  (i -> Text) -> Attribute msg i o
downloadAs_        = Dyn.textProp "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hreflang>
hreflang_ ::  (i -> Text) -> Attribute msg i o
hreflang_          = Dyn.textProp "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/media>
media_ ::  (i -> Text) -> Attribute msg i o
media_             = Dyn.textProp "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ping>
ping_ ::  (i -> Text) -> Attribute msg i o
ping_              = Dyn.textProp "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rel>
rel_ ::  (i -> Text) -> Attribute msg i o
rel_               = Dyn.textProp "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ismap>
ismap_ ::  (i -> Text) -> Attribute msg i o
ismap_             = Dyn.textProp "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/usemap>
usemap_ ::  (i -> Text) -> Attribute msg i o
usemap_            = Dyn.textProp "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/shape>
shape_ ::  (i -> Text) -> Attribute msg i o
shape_             = Dyn.textProp "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/coords>
coords_ ::  (i -> Text) -> Attribute msg i o
coords_            = Dyn.textProp "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/src>
src_ ::  (i -> Text) -> Attribute msg i o
src_               = Dyn.textProp "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/height>
height_ ::  (i -> Text) -> Attribute msg i o
height_            = Dyn.textProp "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/width>
width_ ::  (i -> Text) -> Attribute msg i o
width_             = Dyn.textProp "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/alt>
alt_ ::  (i -> Text) -> Attribute msg i o
alt_               = Dyn.textProp "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autoplay>
autoplay_ ::  (i -> Bool) -> Attribute msg i o
autoplay_          = Dyn.boolProp "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/controls>
controls_ ::  (i -> Bool) -> Attribute msg i o
controls_          = Dyn.boolProp "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/loop>
loop_ ::  (i -> Bool) -> Attribute msg i o
loop_              = Dyn.boolProp "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/preload>
preload_ ::  (i -> Text) -> Attribute msg i o
preload_           = Dyn.textProp "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/poster>
poster_ ::  (i -> Text) -> Attribute msg i o
poster_            = Dyn.textProp "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/default>
default_ ::  (i -> Bool) -> Attribute msg i o
default_           = Dyn.boolProp "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/kind>
kind_ ::  (i -> Text) -> Attribute msg i o
kind_              = Dyn.textProp "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srclang>
srclang_ ::  (i -> Text) -> Attribute msg i o
srclang_           = Dyn.textProp "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/sandbox>
sandbox_ ::  (i -> Text) -> Attribute msg i o
sandbox_           = Dyn.textProp "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/seamless>
seamless_ ::  (i -> Text) -> Attribute msg i o
seamless_          = Dyn.textProp "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srcdoc>
srcdoc_ ::  (i -> Text) -> Attribute msg i o
srcdoc_            = Dyn.textProp "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/reversed>
reversed_ ::  (i -> Text) -> Attribute msg i o
reversed_          = Dyn.textProp "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/start>
start_ ::  (i -> Text) -> Attribute msg i o
start_             = Dyn.textProp "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/align>
align_ ::  (i -> Text) -> Attribute msg i o
align_             = Dyn.textProp "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/colspan>
colspan_ ::  (i -> Text) -> Attribute msg i o
colspan_           = Dyn.textProp "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rowspan>
rowspan_ ::  (i -> Text) -> Attribute msg i o
rowspan_           = Dyn.textProp "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/headers>
headers_ ::  (i -> Text) -> Attribute msg i o
headers_           = Dyn.textProp "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scope>
scope_ ::  (i -> Text) -> Attribute msg i o
scope_             = Dyn.textProp "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/async>
async_ ::  (i -> Text) -> Attribute msg i o
async_             = Dyn.textProp "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/charset>
charset_ ::  (i -> Text) -> Attribute msg i o
charset_           = Dyn.textProp "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/content>
content_ ::  (i -> Text) -> Attribute msg i o
content_           = Dyn.textProp "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defer>
defer_ ::  (i -> Text) -> Attribute msg i o
defer_             = Dyn.textProp "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/httpEquiv>
httpEquiv_ ::  (i -> Text) -> Attribute msg i o
httpEquiv_         = Dyn.textProp "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/language>
language_ ::  (i -> Text) -> Attribute msg i o
language_          = Dyn.textProp "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scoped>
scoped_ ::  (i -> Text) -> Attribute msg i o
scoped_            = Dyn.textProp "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/type>
type_ ::  (i -> Text) -> Attribute msg i o
type_ = Dyn.textProp "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/name>
name_ ::  (i -> Text) -> Attribute msg i o
name_ = Dyn.textProp "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/href>
href_ ::  (i -> Text) -> Attribute msg i o
href_ = Dyn.textProp "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/id>
id_ ::  (i -> Text) -> Attribute msg i o
id_ = Dyn.textProp "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/placeholder>
placeholder_ ::  (i -> Text) -> Attribute msg i o
placeholder_ = Dyn.textProp "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/checked>
checked_ ::  (i -> Bool) -> Attribute msg i o
checked_ = Dyn.boolProp "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autofocus>
autofocus_ ::  (i -> Bool) -> Attribute msg i o
autofocus_ = Dyn.boolProp "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  (i -> Text) -> Attribute msg i o
class_ = Dyn.textProp "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  Text -> (i -> Text) -> Attribute msg i o
data_ k v = Dyn.textProp ("data-" <> k) v

unsafeInnerHTML :: (i -> Text) -> Attribute msg i o
unsafeInnerHTML = Dyn.textProp "innerHTML"
