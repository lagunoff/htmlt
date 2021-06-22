-- |Borrowed from @Miso.HtmlT.Property@
-- https://github.com/dmjio/miso/blob/69f50b49adbff8217f8b51ae5f47727ee950f204/src/Miso/HtmlT/Property.hs
module HtmlT.Property where

import Data.Text

import HtmlT.Types
import HtmlT.Base
import HtmlT.Event


dynStyles :: Dynamic Text -> Html ()
dynStyles = dynProp "style"

dynValue :: Dynamic Text -> Html ()
dynValue = dynProp "value"

dynClass :: Dynamic Text -> Html ()
dynClass = dynProp "className"

dynChecked :: Dynamic Bool -> Html ()
dynChecked = dynProp "checked"

dynDisabled :: Dynamic Bool -> Html ()
dynDisabled = dynProp "disabled"

title_ :: Text -> Html ()
title_ = prop "title"

selected_ :: Bool -> Html ()
selected_ = prop "selected"

hidden_ :: Bool -> Html ()
hidden_ = prop "hidden"

value_ :: Text -> Html ()
value_ = prop "value"

defaultValue_ :: Text -> Html ()
defaultValue_ = prop "defaultValue"

accept_ :: Text -> Html ()
accept_ = prop "accept"

acceptCharset_ :: Text -> Html ()
acceptCharset_ = prop "acceptCharset"

action_ :: Text -> Html ()
action_ = prop "action"

autocomplete_ :: Bool -> Html ()
autocomplete_ b = prop @Text "autocomplete" (if b then "on" else "off")

autosave_ :: Text -> Html ()
autosave_ = prop "autosave"

disabled_ :: Bool -> Html ()
disabled_ = prop "disabled"

enctype_ :: Text -> Html ()
enctype_ = prop "enctype"

formation_ :: Text -> Html ()
formation_ = prop "formation"

list_ :: Text -> Html ()
list_ = prop "list"

maxlength_ :: Text -> Html ()
maxlength_ = prop "maxlength"

minlength_ :: Text -> Html ()
minlength_ = prop "minlength"

method_ :: Text -> Html ()
method_ = prop "method"

multiple_ :: Bool -> Html ()
multiple_ = prop "multiple"

novalidate_ :: Bool -> Html ()
novalidate_ = prop "noValidate"

pattern_ :: Text -> Html ()
pattern_ = prop "pattern"

readonly_ :: Bool -> Html ()
readonly_ = prop "readOnly"

required_ :: Bool -> Html ()
required_ = prop "required"

size_ :: Text -> Html ()
size_ = prop "size"

forProp_ :: Text -> Html ()
forProp_ = prop "for"

ref_ :: Text -> Html ()
ref_ = prop "ref"

formProp_ :: Text -> Html ()
formProp_ = prop "form"

max_ :: Text -> Html ()
max_ = prop "max"

min_ :: Text -> Html ()
min_ = prop "min"

step_ :: Text -> Html ()
step_ = prop "step"

cols_ :: Text -> Html ()
cols_ = prop "cols"

rows_ :: Text -> Html ()
rows_ = prop "rows"

wrap_ :: Text -> Html ()
wrap_ = prop "wrap"

target_ :: Text -> Html ()
target_ = prop "target"

download_ :: Text -> Html ()
download_ = prop "download"

downloadAs_ :: Text -> Html ()
downloadAs_ = prop "downloadAs"

hreflang_ :: Text -> Html ()
hreflang_ = prop "hreflang"

media_ :: Text -> Html ()
media_ = prop "media"

ping_ :: Text -> Html ()
ping_ = prop "ping"

rel_ :: Text -> Html ()
rel_ = prop "rel"

ismap_ :: Text -> Html ()
ismap_ = prop "ismap"

usemap_ :: Text -> Html ()
usemap_ = prop "usemap"

shape_ :: Text -> Html ()
shape_ = prop "shape"

coords_ :: Text -> Html ()
coords_ = prop "coords"

src_ :: Text -> Html ()
src_ = prop "src"

height_ :: Text -> Html ()
height_ = prop "height"

width_ :: Text -> Html ()
width_ = prop "width"

alt_ :: Text -> Html ()
alt_ = prop "alt"

autoplay_ :: Bool -> Html ()
autoplay_ = prop "autoplay"

controls_ :: Bool -> Html ()
controls_ = prop "controls"

loop_ :: Bool -> Html ()
loop_ = prop "loop"

preload_ :: Text -> Html ()
preload_ = prop "preload"

poster_ :: Text -> Html ()
poster_ = prop "poster"

default_ :: Bool -> Html ()
default_ = prop "default"

kind_ :: Text -> Html ()
kind_ = prop "kind"

srclang_ :: Text -> Html ()
srclang_ = prop "srclang"

sandbox_ :: Text -> Html ()
sandbox_ = prop "sandbox"

seamless_ :: Text -> Html ()
seamless_ = prop "seamless"

srcdoc_ :: Text -> Html ()
srcdoc_ = prop "srcdoc"

reversed_ :: Text -> Html ()
reversed_ = prop "reversed"

start_ :: Text -> Html ()
start_ = prop "start"

align_ :: Text -> Html ()
align_ = prop "align"

colspan_ :: Text -> Html ()
colspan_ = prop "colspan"

rowspan_ :: Text -> Html ()
rowspan_ = prop "rowspan"

headers_ :: Text -> Html ()
headers_ = prop "headers"

scope_ :: Text -> Html ()
scope_ = prop "scope"

async_ :: Text -> Html ()
async_ = prop "async"

charset_ :: Text -> Html ()
charset_ = prop "charset"

content_ :: Text -> Html ()
content_ = prop "content"

defer_ :: Text -> Html ()
defer_ = prop "defer"

httpEquiv_ :: Text -> Html ()
httpEquiv_ = prop "httpEquiv"

language_ :: Text -> Html ()
language_ = prop "language"

scoped_ :: Text -> Html ()
scoped_ = prop "scoped"

type_ :: Text -> Html ()
type_ = prop "type"

name_ :: Text -> Html ()
name_ = prop "name"

href_ :: Text -> Html ()
href_ = prop "href"

id_ :: Text -> Html ()
id_ = prop "id"

placeholder_ :: Text -> Html ()
placeholder_ = prop "placeholder"

checked_ :: Bool -> Html ()
checked_ = prop "checked"

autofocus_ :: Bool -> Html ()
autofocus_ = prop "autofocus"

class_ :: Text -> Html ()
class_ = prop "className"

data_ :: Text -> Text -> Html ()
data_ k v = prop @Text ("data-" <> k) v

role_ :: Text -> Html ()
role_ = attr "role"

style_ :: Text -> Html ()
style_ = prop "style"
