-- |Borrowed from @Miso.Html.Property@
-- https://github.com/dmjio/miso/blob/69f50b49adbff8217f8b51ae5f47727ee950f204/src/Miso/Html/Property.hs
module Massaraksh.Property where

import Massaraksh.Types
import Data.Bool
import Massaraksh.Base
import Massaraksh.Event
import Data.Text

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/title>
title_ ::  Text -> Html ()
title_ = prop "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/selected>
selected_ ::  Bool -> Html ()
selected_ = prop "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hidden>
hidden_ ::  Bool -> Html ()
hidden_             = prop "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/value>
value_ ::  Text -> Html ()
value_             = prop "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defaultValue>
defaultValue_ ::  Text -> Html ()
defaultValue_      = prop "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/accept>
accept_ ::  Text -> Html ()
accept_            = prop "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/acceptCharset>
acceptCharset_ ::  Text -> Html ()
acceptCharset_     = prop "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/action>
action_ ::  Text -> Html ()
action_            = prop "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autocomplete>
autocomplete_ ::  Bool -> Html ()
autocomplete_ b = prop @Text "autocomplete" (if b then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autosave>
autosave_ ::  Text -> Html ()
autosave_          = prop "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/disabled>
disabled_ ::  Bool -> Html ()
disabled_          = prop "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/enctype>
enctype_ ::  Text -> Html ()
enctype_           = prop "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/formation>
formation_ ::  Text -> Html ()
formation_         = prop "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/list>
list_ ::  Text -> Html ()
list_              = prop "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/maxlength>
maxlength_ ::  Text -> Html ()
maxlength_         = prop "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/minlength>
minlength_ ::  Text -> Html ()
minlength_         = prop "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/method>
method_ ::  Text -> Html ()
method_            = prop "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/multiple>
multiple_ ::  Bool -> Html ()
multiple_          = prop "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/novalidate>
novalidate_ ::  Bool -> Html ()
novalidate_        = prop "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/pattern>
pattern_ ::  Text -> Html ()
pattern_           = prop "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/readonly>
readonly_ ::  Bool -> Html ()
readonly_          = prop "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/required>
required_ ::  Bool -> Html ()
required_          = prop "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/size>
size_ ::  Text -> Html ()
size_              = prop "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/for>
forProp_ ::  Text -> Html ()
forProp_               = prop "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ref>
ref_ ::  Text -> Html ()
ref_               = prop "ref"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/form>
formProp_ ::  Text -> Html ()
formProp_               = prop "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/max>
max_ ::  Text -> Html ()
max_               = prop "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/min>
min_ ::  Text -> Html ()
min_               = prop "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/step>
step_ ::  Text -> Html ()
step_              = prop "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/cols>
cols_ ::  Text -> Html ()
cols_              = prop "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rows>
rows_ ::  Text -> Html ()
rows_              = prop "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/wrap>
wrap_ ::  Text -> Html ()
wrap_              = prop "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/target>
target_ ::  Text -> Html ()
target_            = prop "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/download>
download_ ::  Text -> Html ()
download_          = prop "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/downloadAs>
downloadAs_ ::  Text -> Html ()
downloadAs_        = prop "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hreflang>
hreflang_ ::  Text -> Html ()
hreflang_          = prop "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/media>
media_ ::  Text -> Html ()
media_             = prop "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ping>
ping_ ::  Text -> Html ()
ping_              = prop "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rel>
rel_ ::  Text -> Html ()
rel_               = prop "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ismap>
ismap_ ::  Text -> Html ()
ismap_             = prop "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/usemap>
usemap_ ::  Text -> Html ()
usemap_            = prop "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/shape>
shape_ ::  Text -> Html ()
shape_             = prop "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/coords>
coords_ ::  Text -> Html ()
coords_            = prop "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/src>
src_ ::  Text -> Html ()
src_               = prop "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/height>
height_ ::  Text -> Html ()
height_            = prop "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/width>
width_ ::  Text -> Html ()
width_             = prop "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/alt>
alt_ ::  Text -> Html ()
alt_               = prop "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autoplay>
autoplay_ ::  Bool -> Html ()
autoplay_          = prop "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/controls>
controls_ ::  Bool -> Html ()
controls_          = prop "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/loop>
loop_ ::  Bool -> Html ()
loop_              = prop "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/preload>
preload_ ::  Text -> Html ()
preload_           = prop "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/poster>
poster_ ::  Text -> Html ()
poster_            = prop "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/default>
default_ ::  Bool -> Html ()
default_           = prop "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/kind>
kind_ ::  Text -> Html ()
kind_              = prop "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srclang>
srclang_ ::  Text -> Html ()
srclang_           = prop "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/sandbox>
sandbox_ ::  Text -> Html ()
sandbox_           = prop "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/seamless>
seamless_ ::  Text -> Html ()
seamless_          = prop "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srcdoc>
srcdoc_ ::  Text -> Html ()
srcdoc_            = prop "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/reversed>
reversed_ ::  Text -> Html ()
reversed_          = prop "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/start>
start_ ::  Text -> Html ()
start_             = prop "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/align>
align_ ::  Text -> Html ()
align_             = prop "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/colspan>
colspan_ ::  Text -> Html ()
colspan_           = prop "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rowspan>
rowspan_ ::  Text -> Html ()
rowspan_           = prop "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/headers>
headers_ ::  Text -> Html ()
headers_           = prop "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scope>
scope_ ::  Text -> Html ()
scope_             = prop "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/async>
async_ ::  Text -> Html ()
async_             = prop "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/charset>
charset_ ::  Text -> Html ()
charset_           = prop "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/content>
content_ ::  Text -> Html ()
content_           = prop "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defer>
defer_ ::  Text -> Html ()
defer_             = prop "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/httpEquiv>
httpEquiv_ ::  Text -> Html ()
httpEquiv_         = prop "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/language>
language_ ::  Text -> Html ()
language_          = prop "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scoped>
scoped_ ::  Text -> Html ()
scoped_            = prop "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/type>
type_ ::  Text -> Html ()
type_ = prop "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/name>
name_ ::  Text -> Html ()
name_ = prop "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/href>
href_ ::  Text -> Html ()
href_ = prop "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/id>
id_ ::  Text -> Html ()
id_ = prop "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/placeholder>
placeholder_ ::  Text -> Html ()
placeholder_ = prop "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/checked>
checked_ ::  Bool -> Html ()
checked_ = prop "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autofocus>
autofocus_ ::  Bool -> Html ()
autofocus_ = prop "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  Text -> Html ()
class_ = prop "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  Text -> Text -> Html ()
data_ k v = prop @Text ("data-" <> k) v
-- | Set "role" attribute
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/role>
role_ ::  Text -> Html ()
role_ = attr "role"

style_ ::  Text -> Html ()
style_ = prop "style"

dynStyle :: Dynamic Text -> Html ()
dynStyle = dynProp "style"

dynValue :: Dynamic Text -> Html ()
dynValue = dynProp "value"

dynClass :: Dynamic Text -> Html ()
dynClass = dynProp "className"

dynChecked :: Dynamic Bool -> Html ()
dynChecked = dynProp "checked"

dynDisabled :: Dynamic Bool -> Html ()
dynDisabled = dynProp "disabled"
