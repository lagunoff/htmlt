{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.Html.Attrs.Dynamic where

import Data.Text (Text)
import qualified Data.Text as JSS
import Massaraksh.Html.Core
import Data.Foldable (foldl')

-- |This module is supposed to be used within a qualified namespace,
-- so it makes sense here to reexport 'textDyn'
--
-- >>> import qualified Massaraksh.Html.Attrs.Dynamic as Dyn
-- >>> h1_ [] [ Dyn.text \i -> "Hello, " <> username i ]
text = textDyn

-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null . _items) ] [ ]
classList_ :: [(Text, i -> Bool)] -> Attribute msg i o
classList_ classes = class_ \i -> JSS.unwords $ foldl' (\acc (cs, f) -> if f i then cs:acc else acc) [] classes
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/style>
style_ :: (i -> Text) -> Attribute msg i o
style_ = textPropDyn "style"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/title>
title_ ::  (i -> Text) -> Attribute msg i o
title_ = textPropDyn "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/selected>
selected_ ::  (i -> Bool) -> Attribute msg i o
selected_ = boolPropDyn "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hidden>
hidden_ ::  (i -> Bool) -> Attribute msg i o
hidden_             = boolPropDyn "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/value>
value_ ::  (i -> Text) -> Attribute msg i o
value_             = textPropDyn "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defaultValue>
defaultValue_ ::  (i -> Text) -> Attribute msg i o
defaultValue_      = textPropDyn "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/accept>
accept_ ::  (i -> Text) -> Attribute msg i o
accept_            = textPropDyn "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/acceptCharset>
acceptCharset_ ::  (i -> Text) -> Attribute msg i o
acceptCharset_     = textPropDyn "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/action>
action_ ::  (i -> Text) -> Attribute msg i o
action_            = textPropDyn "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autocomplete>
autocomplete_ ::  (i -> Bool) -> Attribute msg i o
autocomplete_ b = textPropDyn "autocomplete" (\i -> if b i then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autosave>
autosave_ ::  (i -> Text) -> Attribute msg i o
autosave_          = textPropDyn "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/disabled>
disabled_ ::  (i -> Bool) -> Attribute msg i o
disabled_          = boolPropDyn "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/enctype>
enctype_ ::  (i -> Text) -> Attribute msg i o
enctype_           = textPropDyn "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/formation>
formation_ ::  (i -> Text) -> Attribute msg i o
formation_         = textPropDyn "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/list>
list_ ::  (i -> Text) -> Attribute msg i o
list_              = textPropDyn "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/maxlength>
maxlength_ ::  (i -> Text) -> Attribute msg i o
maxlength_         = textPropDyn "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/minlength>
minlength_ ::  (i -> Text) -> Attribute msg i o
minlength_         = textPropDyn "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/method>
method_ ::  (i -> Text) -> Attribute msg i o
method_            = textPropDyn "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/multiple>
multiple_ ::  (i -> Bool) -> Attribute msg i o
multiple_          = boolPropDyn "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/novalidate>
novalidate_ ::  (i -> Bool) -> Attribute msg i o
novalidate_        = boolPropDyn "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/pattern>
pattern_ ::  (i -> Text) -> Attribute msg i o
pattern_           = textPropDyn "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/readonly>
readonly_ ::  (i -> Bool) -> Attribute msg i o
readonly_          = boolPropDyn "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/required>
required_ ::  (i -> Bool) -> Attribute msg i o
required_          = boolPropDyn "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/size>
size_ ::  (i -> Text) -> Attribute msg i o
size_              = textPropDyn "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/for>
htmlFor_ ::  (i -> Text) -> Attribute msg i o
htmlFor_               = textPropDyn "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/form>
form_ ::  (i -> Text) -> Attribute msg i o
form_               = textPropDyn "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/max>
max_ ::  (i -> Text) -> Attribute msg i o
max_               = textPropDyn "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/min>
min_ ::  (i -> Text) -> Attribute msg i o
min_               = textPropDyn "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/step>
step_ ::  (i -> Text) -> Attribute msg i o
step_              = textPropDyn "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/cols>
cols_ ::  (i -> Text) -> Attribute msg i o
cols_              = textPropDyn "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rows>
rows_ ::  (i -> Text) -> Attribute msg i o
rows_              = textPropDyn "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/wrap>
wrap_ ::  (i -> Text) -> Attribute msg i o
wrap_              = textPropDyn "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/target>
target_ ::  (i -> Text) -> Attribute msg i o
target_            = textPropDyn "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/download>
download_ ::  (i -> Text) -> Attribute msg i o
download_          = textPropDyn "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/downloadAs>
downloadAs_ ::  (i -> Text) -> Attribute msg i o
downloadAs_        = textPropDyn "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hreflang>
hreflang_ ::  (i -> Text) -> Attribute msg i o
hreflang_          = textPropDyn "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/media>
media_ ::  (i -> Text) -> Attribute msg i o
media_             = textPropDyn "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ping>
ping_ ::  (i -> Text) -> Attribute msg i o
ping_              = textPropDyn "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rel>
rel_ ::  (i -> Text) -> Attribute msg i o
rel_               = textPropDyn "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ismap>
ismap_ ::  (i -> Text) -> Attribute msg i o
ismap_             = textPropDyn "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/usemap>
usemap_ ::  (i -> Text) -> Attribute msg i o
usemap_            = textPropDyn "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/shape>
shape_ ::  (i -> Text) -> Attribute msg i o
shape_             = textPropDyn "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/coords>
coords_ ::  (i -> Text) -> Attribute msg i o
coords_            = textPropDyn "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/src>
src_ ::  (i -> Text) -> Attribute msg i o
src_               = textPropDyn "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/height>
height_ ::  (i -> Text) -> Attribute msg i o
height_            = textPropDyn "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/width>
width_ ::  (i -> Text) -> Attribute msg i o
width_             = textPropDyn "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/alt>
alt_ ::  (i -> Text) -> Attribute msg i o
alt_               = textPropDyn "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autoplay>
autoplay_ ::  (i -> Bool) -> Attribute msg i o
autoplay_          = boolPropDyn "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/controls>
controls_ ::  (i -> Bool) -> Attribute msg i o
controls_          = boolPropDyn "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/loop>
loop_ ::  (i -> Bool) -> Attribute msg i o
loop_              = boolPropDyn "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/preload>
preload_ ::  (i -> Text) -> Attribute msg i o
preload_           = textPropDyn "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/poster>
poster_ ::  (i -> Text) -> Attribute msg i o
poster_            = textPropDyn "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/default>
default_ ::  (i -> Bool) -> Attribute msg i o
default_           = boolPropDyn "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/kind>
kind_ ::  (i -> Text) -> Attribute msg i o
kind_              = textPropDyn "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srclang>
srclang_ ::  (i -> Text) -> Attribute msg i o
srclang_           = textPropDyn "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/sandbox>
sandbox_ ::  (i -> Text) -> Attribute msg i o
sandbox_           = textPropDyn "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/seamless>
seamless_ ::  (i -> Text) -> Attribute msg i o
seamless_          = textPropDyn "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srcdoc>
srcdoc_ ::  (i -> Text) -> Attribute msg i o
srcdoc_            = textPropDyn "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/reversed>
reversed_ ::  (i -> Text) -> Attribute msg i o
reversed_          = textPropDyn "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/start>
start_ ::  (i -> Text) -> Attribute msg i o
start_             = textPropDyn "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/align>
align_ ::  (i -> Text) -> Attribute msg i o
align_             = textPropDyn "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/colspan>
colspan_ ::  (i -> Text) -> Attribute msg i o
colspan_           = textPropDyn "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rowspan>
rowspan_ ::  (i -> Text) -> Attribute msg i o
rowspan_           = textPropDyn "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/headers>
headers_ ::  (i -> Text) -> Attribute msg i o
headers_           = textPropDyn "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scope>
scope_ ::  (i -> Text) -> Attribute msg i o
scope_             = textPropDyn "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/async>
async_ ::  (i -> Text) -> Attribute msg i o
async_             = textPropDyn "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/charset>
charset_ ::  (i -> Text) -> Attribute msg i o
charset_           = textPropDyn "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/content>
content_ ::  (i -> Text) -> Attribute msg i o
content_           = textPropDyn "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defer>
defer_ ::  (i -> Text) -> Attribute msg i o
defer_             = textPropDyn "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/httpEquiv>
httpEquiv_ ::  (i -> Text) -> Attribute msg i o
httpEquiv_         = textPropDyn "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/language>
language_ ::  (i -> Text) -> Attribute msg i o
language_          = textPropDyn "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scoped>
scoped_ ::  (i -> Text) -> Attribute msg i o
scoped_            = textPropDyn "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/type>
type_ ::  (i -> Text) -> Attribute msg i o
type_ = textPropDyn "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/name>
name_ ::  (i -> Text) -> Attribute msg i o
name_ = textPropDyn "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/href>
href_ ::  (i -> Text) -> Attribute msg i o
href_ = textPropDyn "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/id>
id_ ::  (i -> Text) -> Attribute msg i o
id_ = textPropDyn "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/placeholder>
placeholder_ ::  (i -> Text) -> Attribute msg i o
placeholder_ = textPropDyn "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/checked>
checked_ ::  (i -> Bool) -> Attribute msg i o
checked_ = boolPropDyn "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autofocus>
autofocus_ ::  (i -> Bool) -> Attribute msg i o
autofocus_ = boolPropDyn "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  (i -> Text) -> Attribute msg i o
class_ = textPropDyn "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  Text -> (i -> Text) -> Attribute msg i o
data_ k v = textPropDyn ("data-" <> k) v

unsafeInnerHTML :: (i -> Text) -> Attribute msg i o
unsafeInnerHTML = textPropDyn "innerHTML"
