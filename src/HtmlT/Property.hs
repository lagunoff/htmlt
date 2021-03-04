-- |Borrowed from @Miso.HtmlT.Property@
-- https://github.com/dmjio/miso/blob/69f50b49adbff8217f8b51ae5f47727ee950f204/src/Miso/HtmlT/Property.hs
module HtmlT.Property where

import Data.Text

import HtmlT.Types
import HtmlT.Base
import HtmlT.Event
import HtmlT.Internal


dynStyles :: Dynamic Text -> HtmlT ()
dynStyles = dynProp "style"

dynValue :: Dynamic Text -> HtmlT ()
dynValue = dynProp "value"

dynClass :: Dynamic Text -> HtmlT ()
dynClass = dynProp "className"

dynChecked :: Dynamic Bool -> HtmlT ()
dynChecked = dynProp "checked"

dynDisabled :: Dynamic Bool -> HtmlT ()
dynDisabled = dynProp "disabled"

title_ ::  Text -> HtmlT ()
title_ = prop "title"

selected_ ::  Bool -> HtmlT ()
selected_ = prop "selected"

hidden_ ::  Bool -> HtmlT ()
hidden_             = prop "hidden"

value_ ::  Text -> HtmlT ()
value_             = prop "value"

defaultValue_ ::  Text -> HtmlT ()
defaultValue_      = prop "defaultValue"

accept_ ::  Text -> HtmlT ()
accept_            = prop "accept"

acceptCharset_ ::  Text -> HtmlT ()
acceptCharset_     = prop "acceptCharset"

action_ ::  Text -> HtmlT ()
action_            = prop "action"

autocomplete_ ::  Bool -> HtmlT ()
autocomplete_ b = prop @Text "autocomplete" (if b then "on" else "off")

autosave_ ::  Text -> HtmlT ()
autosave_          = prop "autosave"

disabled_ ::  Bool -> HtmlT ()
disabled_          = prop "disabled"

enctype_ ::  Text -> HtmlT ()
enctype_           = prop "enctype"

formation_ ::  Text -> HtmlT ()
formation_         = prop "formation"

list_ ::  Text -> HtmlT ()
list_              = prop "list"

maxlength_ ::  Text -> HtmlT ()
maxlength_         = prop "maxlength"

minlength_ ::  Text -> HtmlT ()
minlength_         = prop "minlength"

method_ ::  Text -> HtmlT ()
method_            = prop "method"

multiple_ ::  Bool -> HtmlT ()
multiple_          = prop "multiple"

novalidate_ ::  Bool -> HtmlT ()
novalidate_        = prop "noValidate"

pattern_ ::  Text -> HtmlT ()
pattern_           = prop "pattern"

readonly_ ::  Bool -> HtmlT ()
readonly_          = prop "readOnly"

required_ ::  Bool -> HtmlT ()
required_          = prop "required"

size_ ::  Text -> HtmlT ()
size_              = prop "size"

forProp_ ::  Text -> HtmlT ()
forProp_               = prop "for"

ref_ ::  Text -> HtmlT ()
ref_               = prop "ref"

formProp_ ::  Text -> HtmlT ()
formProp_               = prop "form"

max_ ::  Text -> HtmlT ()
max_               = prop "max"

min_ ::  Text -> HtmlT ()
min_               = prop "min"

step_ ::  Text -> HtmlT ()
step_              = prop "step"

cols_ ::  Text -> HtmlT ()
cols_              = prop "cols"

rows_ ::  Text -> HtmlT ()
rows_              = prop "rows"

wrap_ ::  Text -> HtmlT ()
wrap_              = prop "wrap"

target_ ::  Text -> HtmlT ()
target_            = prop "target"

download_ ::  Text -> HtmlT ()
download_          = prop "download"

downloadAs_ ::  Text -> HtmlT ()
downloadAs_        = prop "downloadAs"

hreflang_ ::  Text -> HtmlT ()
hreflang_          = prop "hreflang"

media_ ::  Text -> HtmlT ()
media_             = prop "media"

ping_ ::  Text -> HtmlT ()
ping_              = prop "ping"

rel_ ::  Text -> HtmlT ()
rel_               = prop "rel"

ismap_ ::  Text -> HtmlT ()
ismap_             = prop "ismap"

usemap_ ::  Text -> HtmlT ()
usemap_            = prop "usemap"

shape_ ::  Text -> HtmlT ()
shape_             = prop "shape"

coords_ ::  Text -> HtmlT ()
coords_            = prop "coords"

src_ ::  Text -> HtmlT ()
src_               = prop "src"

height_ ::  Text -> HtmlT ()
height_            = prop "height"

width_ ::  Text -> HtmlT ()
width_             = prop "width"

alt_ ::  Text -> HtmlT ()
alt_               = prop "alt"

autoplay_ ::  Bool -> HtmlT ()
autoplay_          = prop "autoplay"

controls_ ::  Bool -> HtmlT ()
controls_          = prop "controls"

loop_ ::  Bool -> HtmlT ()
loop_              = prop "loop"

preload_ ::  Text -> HtmlT ()
preload_           = prop "preload"

poster_ ::  Text -> HtmlT ()
poster_            = prop "poster"

default_ ::  Bool -> HtmlT ()
default_           = prop "default"

kind_ ::  Text -> HtmlT ()
kind_              = prop "kind"

srclang_ ::  Text -> HtmlT ()
srclang_           = prop "srclang"

sandbox_ ::  Text -> HtmlT ()
sandbox_           = prop "sandbox"

seamless_ ::  Text -> HtmlT ()
seamless_          = prop "seamless"

srcdoc_ ::  Text -> HtmlT ()
srcdoc_            = prop "srcdoc"

reversed_ ::  Text -> HtmlT ()
reversed_          = prop "reversed"

start_ ::  Text -> HtmlT ()
start_             = prop "start"

align_ ::  Text -> HtmlT ()
align_             = prop "align"

colspan_ ::  Text -> HtmlT ()
colspan_           = prop "colspan"

rowspan_ ::  Text -> HtmlT ()
rowspan_           = prop "rowspan"

headers_ ::  Text -> HtmlT ()
headers_           = prop "headers"

scope_ ::  Text -> HtmlT ()
scope_             = prop "scope"

async_ ::  Text -> HtmlT ()
async_             = prop "async"

charset_ ::  Text -> HtmlT ()
charset_           = prop "charset"

content_ ::  Text -> HtmlT ()
content_           = prop "content"

defer_ ::  Text -> HtmlT ()
defer_             = prop "defer"

httpEquiv_ ::  Text -> HtmlT ()
httpEquiv_         = prop "httpEquiv"

language_ ::  Text -> HtmlT ()
language_          = prop "language"

scoped_ ::  Text -> HtmlT ()
scoped_            = prop "scoped"

type_ ::  Text -> HtmlT ()
type_ = prop "type"

name_ ::  Text -> HtmlT ()
name_ = prop "name"

href_ ::  Text -> HtmlT ()
href_ = prop "href"

id_ ::  Text -> HtmlT ()
id_ = prop "id"

placeholder_ ::  Text -> HtmlT ()
placeholder_ = prop "placeholder"

checked_ ::  Bool -> HtmlT ()
checked_ = prop "checked"

autofocus_ ::  Bool -> HtmlT ()
autofocus_ = prop "autofocus"

class_ ::  Text -> HtmlT ()
class_ = prop "className"

data_ ::  Text -> Text -> HtmlT ()
data_ k v = prop @Text ("data-" <> k) v

role_ ::  Text -> HtmlT ()
role_ = attr "role"

style_ ::  Text -> HtmlT ()
style_ = prop "style"
