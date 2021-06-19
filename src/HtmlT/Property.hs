-- |Borrowed from @Miso.HtmlT.Property@
-- https://github.com/dmjio/miso/blob/69f50b49adbff8217f8b51ae5f47727ee950f204/src/Miso/HtmlT/Property.hs
module HtmlT.Property where

import Control.Monad.IO.Class
import Data.Text

import HtmlT.Types
import HtmlT.Base
import HtmlT.Event


dynStyles :: MonadIO m => Dynamic Text -> HtmlT m ()
dynStyles = dynProp "style"

dynValue :: MonadIO m => Dynamic Text -> HtmlT m ()
dynValue = dynProp "value"

dynClass :: MonadIO m => Dynamic Text -> HtmlT m ()
dynClass = dynProp "className"

dynChecked :: MonadIO m => Dynamic Bool -> HtmlT m ()
dynChecked = dynProp "checked"

dynDisabled :: MonadIO m => Dynamic Bool -> HtmlT m ()
dynDisabled = dynProp "disabled"

title_ :: MonadIO m => Text -> HtmlT m ()
title_ = prop "title"

selected_ :: MonadIO m => Bool -> HtmlT m ()
selected_ = prop "selected"

hidden_ :: MonadIO m => Bool -> HtmlT m ()
hidden_ = prop "hidden"

value_ :: MonadIO m => Text -> HtmlT m ()
value_ = prop "value"

defaultValue_ :: MonadIO m => Text -> HtmlT m ()
defaultValue_ = prop "defaultValue"

accept_ :: MonadIO m => Text -> HtmlT m ()
accept_ = prop "accept"

acceptCharset_ :: MonadIO m => Text -> HtmlT m ()
acceptCharset_ = prop "acceptCharset"

action_ :: MonadIO m => Text -> HtmlT m ()
action_ = prop "action"

autocomplete_ :: MonadIO m => Bool -> HtmlT m ()
autocomplete_ b = prop @Text "autocomplete" (if b then "on" else "off")

autosave_ :: MonadIO m => Text -> HtmlT m ()
autosave_ = prop "autosave"

disabled_ :: MonadIO m => Bool -> HtmlT m ()
disabled_ = prop "disabled"

enctype_ :: MonadIO m => Text -> HtmlT m ()
enctype_ = prop "enctype"

formation_ :: MonadIO m => Text -> HtmlT m ()
formation_ = prop "formation"

list_ :: MonadIO m => Text -> HtmlT m ()
list_ = prop "list"

maxlength_ :: MonadIO m => Text -> HtmlT m ()
maxlength_ = prop "maxlength"

minlength_ :: MonadIO m => Text -> HtmlT m ()
minlength_ = prop "minlength"

method_ :: MonadIO m => Text -> HtmlT m ()
method_ = prop "method"

multiple_ :: MonadIO m => Bool -> HtmlT m ()
multiple_ = prop "multiple"

novalidate_ :: MonadIO m => Bool -> HtmlT m ()
novalidate_ = prop "noValidate"

pattern_ :: MonadIO m => Text -> HtmlT m ()
pattern_ = prop "pattern"

readonly_ :: MonadIO m => Bool -> HtmlT m ()
readonly_ = prop "readOnly"

required_ :: MonadIO m => Bool -> HtmlT m ()
required_ = prop "required"

size_ :: MonadIO m => Text -> HtmlT m ()
size_ = prop "size"

forProp_ :: MonadIO m => Text -> HtmlT m ()
forProp_ = prop "for"

ref_ :: MonadIO m => Text -> HtmlT m ()
ref_ = prop "ref"

formProp_ :: MonadIO m => Text -> HtmlT m ()
formProp_ = prop "form"

max_ :: MonadIO m => Text -> HtmlT m ()
max_ = prop "max"

min_ :: MonadIO m => Text -> HtmlT m ()
min_ = prop "min"

step_ :: MonadIO m => Text -> HtmlT m ()
step_ = prop "step"

cols_ :: MonadIO m => Text -> HtmlT m ()
cols_ = prop "cols"

rows_ :: MonadIO m => Text -> HtmlT m ()
rows_ = prop "rows"

wrap_ :: MonadIO m => Text -> HtmlT m ()
wrap_ = prop "wrap"

target_ :: MonadIO m => Text -> HtmlT m ()
target_ = prop "target"

download_ :: MonadIO m => Text -> HtmlT m ()
download_ = prop "download"

downloadAs_ :: MonadIO m => Text -> HtmlT m ()
downloadAs_ = prop "downloadAs"

hreflang_ :: MonadIO m => Text -> HtmlT m ()
hreflang_ = prop "hreflang"

media_ :: MonadIO m => Text -> HtmlT m ()
media_ = prop "media"

ping_ :: MonadIO m => Text -> HtmlT m ()
ping_ = prop "ping"

rel_ :: MonadIO m => Text -> HtmlT m ()
rel_ = prop "rel"

ismap_ :: MonadIO m => Text -> HtmlT m ()
ismap_ = prop "ismap"

usemap_ :: MonadIO m => Text -> HtmlT m ()
usemap_ = prop "usemap"

shape_ :: MonadIO m => Text -> HtmlT m ()
shape_ = prop "shape"

coords_ :: MonadIO m => Text -> HtmlT m ()
coords_ = prop "coords"

src_ :: MonadIO m => Text -> HtmlT m ()
src_ = prop "src"

height_ :: MonadIO m => Text -> HtmlT m ()
height_ = prop "height"

width_ :: MonadIO m => Text -> HtmlT m ()
width_ = prop "width"

alt_ :: MonadIO m => Text -> HtmlT m ()
alt_ = prop "alt"

autoplay_ :: MonadIO m => Bool -> HtmlT m ()
autoplay_ = prop "autoplay"

controls_ :: MonadIO m => Bool -> HtmlT m ()
controls_ = prop "controls"

loop_ :: MonadIO m => Bool -> HtmlT m ()
loop_ = prop "loop"

preload_ :: MonadIO m => Text -> HtmlT m ()
preload_ = prop "preload"

poster_ :: MonadIO m => Text -> HtmlT m ()
poster_ = prop "poster"

default_ :: MonadIO m => Bool -> HtmlT m ()
default_ = prop "default"

kind_ :: MonadIO m => Text -> HtmlT m ()
kind_ = prop "kind"

srclang_ :: MonadIO m => Text -> HtmlT m ()
srclang_ = prop "srclang"

sandbox_ :: MonadIO m => Text -> HtmlT m ()
sandbox_ = prop "sandbox"

seamless_ :: MonadIO m => Text -> HtmlT m ()
seamless_ = prop "seamless"

srcdoc_ :: MonadIO m => Text -> HtmlT m ()
srcdoc_ = prop "srcdoc"

reversed_ :: MonadIO m => Text -> HtmlT m ()
reversed_ = prop "reversed"

start_ :: MonadIO m => Text -> HtmlT m ()
start_ = prop "start"

align_ :: MonadIO m => Text -> HtmlT m ()
align_ = prop "align"

colspan_ :: MonadIO m => Text -> HtmlT m ()
colspan_ = prop "colspan"

rowspan_ :: MonadIO m => Text -> HtmlT m ()
rowspan_ = prop "rowspan"

headers_ :: MonadIO m => Text -> HtmlT m ()
headers_ = prop "headers"

scope_ :: MonadIO m => Text -> HtmlT m ()
scope_ = prop "scope"

async_ :: MonadIO m => Text -> HtmlT m ()
async_ = prop "async"

charset_ :: MonadIO m => Text -> HtmlT m ()
charset_ = prop "charset"

content_ :: MonadIO m => Text -> HtmlT m ()
content_ = prop "content"

defer_ :: MonadIO m => Text -> HtmlT m ()
defer_ = prop "defer"

httpEquiv_ :: MonadIO m => Text -> HtmlT m ()
httpEquiv_ = prop "httpEquiv"

language_ :: MonadIO m => Text -> HtmlT m ()
language_ = prop "language"

scoped_ :: MonadIO m => Text -> HtmlT m ()
scoped_ = prop "scoped"

type_ :: MonadIO m => Text -> HtmlT m ()
type_ = prop "type"

name_ :: MonadIO m => Text -> HtmlT m ()
name_ = prop "name"

href_ :: MonadIO m => Text -> HtmlT m ()
href_ = prop "href"

id_ :: MonadIO m => Text -> HtmlT m ()
id_ = prop "id"

placeholder_ :: MonadIO m => Text -> HtmlT m ()
placeholder_ = prop "placeholder"

checked_ :: MonadIO m => Bool -> HtmlT m ()
checked_ = prop "checked"

autofocus_ :: MonadIO m => Bool -> HtmlT m ()
autofocus_ = prop "autofocus"

class_ :: MonadIO m => Text -> HtmlT m ()
class_ = prop "className"

data_ :: MonadIO m => Text -> Text -> HtmlT m ()
data_ k v = prop @Text ("data-" <> k) v

role_ :: MonadIO m => Text -> HtmlT m ()
role_ = attr "role"

style_ :: MonadIO m => Text -> HtmlT m ()
style_ = prop "style"
