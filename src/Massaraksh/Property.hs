-- |Borrowed from @Miso.HtmlT.Property@
-- https://github.com/dmjio/miso/blob/69f50b49adbff8217f8b51ae5f47727ee950f204/src/Miso/HtmlT/Property.hs
module Massaraksh.Property where

import Massaraksh.Types
import Massaraksh.Base
import Massaraksh.Event
import Data.Text

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/title>
title_ ::  Text -> HtmlT ()
title_ = prop "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/selected>
selected_ ::  Bool -> HtmlT ()
selected_ = prop "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hidden>
hidden_ ::  Bool -> HtmlT ()
hidden_             = prop "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/value>
value_ ::  Text -> HtmlT ()
value_             = prop "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defaultValue>
defaultValue_ ::  Text -> HtmlT ()
defaultValue_      = prop "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/accept>
accept_ ::  Text -> HtmlT ()
accept_            = prop "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/acceptCharset>
acceptCharset_ ::  Text -> HtmlT ()
acceptCharset_     = prop "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/action>
action_ ::  Text -> HtmlT ()
action_            = prop "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autocomplete>
autocomplete_ ::  Bool -> HtmlT ()
autocomplete_ b = prop @Text "autocomplete" (if b then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autosave>
autosave_ ::  Text -> HtmlT ()
autosave_          = prop "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/disabled>
disabled_ ::  Bool -> HtmlT ()
disabled_          = prop "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/enctype>
enctype_ ::  Text -> HtmlT ()
enctype_           = prop "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/formation>
formation_ ::  Text -> HtmlT ()
formation_         = prop "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/list>
list_ ::  Text -> HtmlT ()
list_              = prop "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/maxlength>
maxlength_ ::  Text -> HtmlT ()
maxlength_         = prop "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/minlength>
minlength_ ::  Text -> HtmlT ()
minlength_         = prop "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/method>
method_ ::  Text -> HtmlT ()
method_            = prop "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/multiple>
multiple_ ::  Bool -> HtmlT ()
multiple_          = prop "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/novalidate>
novalidate_ ::  Bool -> HtmlT ()
novalidate_        = prop "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/pattern>
pattern_ ::  Text -> HtmlT ()
pattern_           = prop "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/readonly>
readonly_ ::  Bool -> HtmlT ()
readonly_          = prop "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/required>
required_ ::  Bool -> HtmlT ()
required_          = prop "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/size>
size_ ::  Text -> HtmlT ()
size_              = prop "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/for>
forProp_ ::  Text -> HtmlT ()
forProp_               = prop "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ref>
ref_ ::  Text -> HtmlT ()
ref_               = prop "ref"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/form>
formProp_ ::  Text -> HtmlT ()
formProp_               = prop "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/max>
max_ ::  Text -> HtmlT ()
max_               = prop "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/min>
min_ ::  Text -> HtmlT ()
min_               = prop "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/step>
step_ ::  Text -> HtmlT ()
step_              = prop "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/cols>
cols_ ::  Text -> HtmlT ()
cols_              = prop "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rows>
rows_ ::  Text -> HtmlT ()
rows_              = prop "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/wrap>
wrap_ ::  Text -> HtmlT ()
wrap_              = prop "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/target>
target_ ::  Text -> HtmlT ()
target_            = prop "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/download>
download_ ::  Text -> HtmlT ()
download_          = prop "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/downloadAs>
downloadAs_ ::  Text -> HtmlT ()
downloadAs_        = prop "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hreflang>
hreflang_ ::  Text -> HtmlT ()
hreflang_          = prop "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/media>
media_ ::  Text -> HtmlT ()
media_             = prop "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ping>
ping_ ::  Text -> HtmlT ()
ping_              = prop "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rel>
rel_ ::  Text -> HtmlT ()
rel_               = prop "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ismap>
ismap_ ::  Text -> HtmlT ()
ismap_             = prop "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/usemap>
usemap_ ::  Text -> HtmlT ()
usemap_            = prop "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/shape>
shape_ ::  Text -> HtmlT ()
shape_             = prop "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/coords>
coords_ ::  Text -> HtmlT ()
coords_            = prop "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/src>
src_ ::  Text -> HtmlT ()
src_               = prop "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/height>
height_ ::  Text -> HtmlT ()
height_            = prop "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/width>
width_ ::  Text -> HtmlT ()
width_             = prop "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/alt>
alt_ ::  Text -> HtmlT ()
alt_               = prop "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autoplay>
autoplay_ ::  Bool -> HtmlT ()
autoplay_          = prop "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/controls>
controls_ ::  Bool -> HtmlT ()
controls_          = prop "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/loop>
loop_ ::  Bool -> HtmlT ()
loop_              = prop "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/preload>
preload_ ::  Text -> HtmlT ()
preload_           = prop "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/poster>
poster_ ::  Text -> HtmlT ()
poster_            = prop "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/default>
default_ ::  Bool -> HtmlT ()
default_           = prop "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/kind>
kind_ ::  Text -> HtmlT ()
kind_              = prop "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srclang>
srclang_ ::  Text -> HtmlT ()
srclang_           = prop "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/sandbox>
sandbox_ ::  Text -> HtmlT ()
sandbox_           = prop "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/seamless>
seamless_ ::  Text -> HtmlT ()
seamless_          = prop "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srcdoc>
srcdoc_ ::  Text -> HtmlT ()
srcdoc_            = prop "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/reversed>
reversed_ ::  Text -> HtmlT ()
reversed_          = prop "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/start>
start_ ::  Text -> HtmlT ()
start_             = prop "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/align>
align_ ::  Text -> HtmlT ()
align_             = prop "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/colspan>
colspan_ ::  Text -> HtmlT ()
colspan_           = prop "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rowspan>
rowspan_ ::  Text -> HtmlT ()
rowspan_           = prop "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/headers>
headers_ ::  Text -> HtmlT ()
headers_           = prop "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scope>
scope_ ::  Text -> HtmlT ()
scope_             = prop "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/async>
async_ ::  Text -> HtmlT ()
async_             = prop "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/charset>
charset_ ::  Text -> HtmlT ()
charset_           = prop "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/content>
content_ ::  Text -> HtmlT ()
content_           = prop "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defer>
defer_ ::  Text -> HtmlT ()
defer_             = prop "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/httpEquiv>
httpEquiv_ ::  Text -> HtmlT ()
httpEquiv_         = prop "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/language>
language_ ::  Text -> HtmlT ()
language_          = prop "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scoped>
scoped_ ::  Text -> HtmlT ()
scoped_            = prop "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/type>
type_ ::  Text -> HtmlT ()
type_ = prop "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/name>
name_ ::  Text -> HtmlT ()
name_ = prop "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/href>
href_ ::  Text -> HtmlT ()
href_ = prop "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/id>
id_ ::  Text -> HtmlT ()
id_ = prop "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/placeholder>
placeholder_ ::  Text -> HtmlT ()
placeholder_ = prop "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/checked>
checked_ ::  Bool -> HtmlT ()
checked_ = prop "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autofocus>
autofocus_ ::  Bool -> HtmlT ()
autofocus_ = prop "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  Text -> HtmlT ()
class_ = prop "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  Text -> Text -> HtmlT ()
data_ k v = prop @Text ("data-" <> k) v
-- | Set "role" attribute
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/role>
role_ ::  Text -> HtmlT ()
role_ = attr "role"

style_ ::  Text -> HtmlT ()
style_ = prop "style"

dynStyle :: Dynamic Text -> HtmlT ()
dynStyle = dynProp "style"

dynValue :: Dynamic Text -> HtmlT ()
dynValue = dynProp "value"

dynClass :: Dynamic Text -> HtmlT ()
dynClass = dynProp "className"

dynChecked :: Dynamic Bool -> HtmlT ()
dynChecked = dynProp "checked"

dynDisabled :: Dynamic Bool -> HtmlT ()
dynDisabled = dynProp "disabled"
