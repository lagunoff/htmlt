{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.Html.Attrs where

import Data.Text (Text)
import Massaraksh.Html

-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null . _items) ] [ ]
classList_ :: [(Text, Bool)] -> Attribute msg i o
classList_ _ = noopAttr
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/title>
title_ ::  Text -> Attribute msg i o
title_ = textProp "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/selected>
selected_ ::  Bool -> Attribute msg i o
selected_ = boolProp "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hidden>
hidden_ ::  Bool -> Attribute msg i o
hidden_             = boolProp "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/value>
value_ ::  Text -> Attribute msg i o
value_             = textProp "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defaultValue>
defaultValue_ ::  Text -> Attribute msg i o
defaultValue_      = textProp "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/accept>
accept_ ::  Text -> Attribute msg i o
accept_            = textProp "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/acceptCharset>
acceptCharset_ ::  Text -> Attribute msg i o
acceptCharset_     = textProp "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/action>
action_ ::  Text -> Attribute msg i o
action_            = textProp "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autocomplete>
autocomplete_ ::  Bool -> Attribute msg i o
autocomplete_ b = textProp "autocomplete" (if b then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autosave>
autosave_ ::  Text -> Attribute msg i o
autosave_          = textProp "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/disabled>
disabled_ ::  Bool -> Attribute msg i o
disabled_          = boolProp "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/enctype>
enctype_ ::  Text -> Attribute msg i o
enctype_           = textProp "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/formation>
formation_ ::  Text -> Attribute msg i o
formation_         = textProp "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/list>
list_ ::  Text -> Attribute msg i o
list_              = textProp "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/maxlength>
maxlength_ ::  Text -> Attribute msg i o
maxlength_         = textProp "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/minlength>
minlength_ ::  Text -> Attribute msg i o
minlength_         = textProp "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/method>
method_ ::  Text -> Attribute msg i o
method_            = textProp "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/multiple>
multiple_ ::  Bool -> Attribute msg i o
multiple_          = boolProp "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/novalidate>
novalidate_ ::  Bool -> Attribute msg i o
novalidate_        = boolProp "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/pattern>
pattern_ ::  Text -> Attribute msg i o
pattern_           = textProp "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/readonly>
readonly_ ::  Bool -> Attribute msg i o
readonly_          = boolProp "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/required>
required_ ::  Bool -> Attribute msg i o
required_          = boolProp "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/size>
size_ ::  Text -> Attribute msg i o
size_              = textProp "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/for>
htmlFor_ ::  Text -> Attribute msg i o
htmlFor_               = attr "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/form>
form_ ::  Text -> Attribute msg i o
form_               = textProp "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/max>
max_ ::  Text -> Attribute msg i o
max_               = textProp "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/min>
min_ ::  Text -> Attribute msg i o
min_               = textProp "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/step>
step_ ::  Text -> Attribute msg i o
step_              = textProp "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/cols>
cols_ ::  Text -> Attribute msg i o
cols_              = textProp "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rows>
rows_ ::  Text -> Attribute msg i o
rows_              = textProp "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/wrap>
wrap_ ::  Text -> Attribute msg i o
wrap_              = textProp "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/target>
target_ ::  Text -> Attribute msg i o
target_            = textProp "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/download>
download_ ::  Text -> Attribute msg i o
download_          = textProp "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/downloadAs>
downloadAs_ ::  Text -> Attribute msg i o
downloadAs_        = textProp "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hreflang>
hreflang_ ::  Text -> Attribute msg i o
hreflang_          = textProp "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/media>
media_ ::  Text -> Attribute msg i o
media_             = textProp "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ping>
ping_ ::  Text -> Attribute msg i o
ping_              = textProp "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rel>
rel_ ::  Text -> Attribute msg i o
rel_               = textProp "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ismap>
ismap_ ::  Text -> Attribute msg i o
ismap_             = textProp "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/usemap>
usemap_ ::  Text -> Attribute msg i o
usemap_            = textProp "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/shape>
shape_ ::  Text -> Attribute msg i o
shape_             = textProp "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/coords>
coords_ ::  Text -> Attribute msg i o
coords_            = textProp "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/src>
src_ ::  Text -> Attribute msg i o
src_               = textProp "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/height>
height_ ::  Text -> Attribute msg i o
height_            = textProp "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/width>
width_ ::  Text -> Attribute msg i o
width_             = textProp "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/alt>
alt_ ::  Text -> Attribute msg i o
alt_               = textProp "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autoplay>
autoplay_ ::  Bool -> Attribute msg i o
autoplay_          = boolProp "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/controls>
controls_ ::  Bool -> Attribute msg i o
controls_          = boolProp "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/loop>
loop_ ::  Bool -> Attribute msg i o
loop_              = boolProp "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/preload>
preload_ ::  Text -> Attribute msg i o
preload_           = textProp "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/poster>
poster_ ::  Text -> Attribute msg i o
poster_            = textProp "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/default>
default_ ::  Bool -> Attribute msg i o
default_           = boolProp "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/kind>
kind_ ::  Text -> Attribute msg i o
kind_              = textProp "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srclang>
srclang_ ::  Text -> Attribute msg i o
srclang_           = textProp "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/sandbox>
sandbox_ ::  Text -> Attribute msg i o
sandbox_           = textProp "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/seamless>
seamless_ ::  Text -> Attribute msg i o
seamless_          = textProp "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srcdoc>
srcdoc_ ::  Text -> Attribute msg i o
srcdoc_            = textProp "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/reversed>
reversed_ ::  Text -> Attribute msg i o
reversed_          = textProp "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/start>
start_ ::  Text -> Attribute msg i o
start_             = textProp "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/align>
align_ ::  Text -> Attribute msg i o
align_             = textProp "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/colspan>
colspan_ ::  Text -> Attribute msg i o
colspan_           = textProp "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rowspan>
rowspan_ ::  Text -> Attribute msg i o
rowspan_           = textProp "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/headers>
headers_ ::  Text -> Attribute msg i o
headers_           = textProp "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scope>
scope_ ::  Text -> Attribute msg i o
scope_             = textProp "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/async>
async_ ::  Text -> Attribute msg i o
async_             = textProp "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/charset>
charset_ ::  Text -> Attribute msg i o
charset_           = textProp "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/content>
content_ ::  Text -> Attribute msg i o
content_           = textProp "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defer>
defer_ ::  Text -> Attribute msg i o
defer_             = textProp "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/httpEquiv>
httpEquiv_ ::  Text -> Attribute msg i o
httpEquiv_         = textProp "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/language>
language_ ::  Text -> Attribute msg i o
language_          = textProp "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scoped>
scoped_ ::  Text -> Attribute msg i o
scoped_            = textProp "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/type>
type_ ::  Text -> Attribute msg i o
type_ = textProp "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/name>
name_ ::  Text -> Attribute msg i o
name_ = textProp "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/href>
href_ ::  Text -> Attribute msg i o
href_ = textProp "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/id>
id_ ::  Text -> Attribute msg i o
id_ = textProp "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/placeholder>
placeholder_ ::  Text -> Attribute msg i o
placeholder_ = textProp "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/checked>
checked_ ::  Bool -> Attribute msg i o
checked_ = boolProp "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autofocus>
autofocus_ ::  Bool -> Attribute msg i o
autofocus_ = boolProp "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  Text -> Attribute msg i o
class_ = textProp "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  Text -> Text -> Attribute msg i o
data_ k v = textProp ("data-" <> k) v

