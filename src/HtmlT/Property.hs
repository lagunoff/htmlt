{-|
Shortcuts for common HTML5 attributes and properties
-}
module HtmlT.Property where

import HtmlT.Base
import HtmlT.Event
import HtmlT.Types
import Data.Text
import Wasm.Compat.Prim


-- TODO: Real-world usage has demonstrated that 'dynStyles' not
-- sufficiently composable. For instance, if 'dynStyles' is used to
-- set the CSS color for an element, essentially no other CSS property
-- can be applied to this element, as they will be overwritten by
-- 'dynStyles'.
dynStyles :: Dynamic Text -> Html ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: Dynamic Text -> Html ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: Dynamic Text -> Html ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: Dynamic Bool -> Html ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: Dynamic Bool -> Html ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}

title_ :: Text -> Html ()
title_ = prop "title"
{-# INLINE title_ #-}

selected_ :: Bool -> Html ()
selected_ = prop "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> Html ()
hidden_ = prop "hidden"
{-# INLINE hidden_ #-}

value_ :: Text -> Html ()
value_ = prop "value"
{-# INLINE value_ #-}

defaultValue_ :: Text -> Html ()
defaultValue_ = prop "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: Text -> Html ()
accept_ = prop "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: Text -> Html ()
acceptCharset_ = prop "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: Text -> Html ()
action_ = prop "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> Html ()
autocomplete_ b = prop @Text "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: Text -> Html ()
autosave_ = prop "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> Html ()
disabled_ = prop "disabled"
{-# INLINE disabled_ #-}

enctype_ :: Text -> Html ()
enctype_ = prop "enctype"
{-# INLINE enctype_ #-}

formation_ :: Text -> Html ()
formation_ = prop "formation"
{-# INLINE formation_ #-}

list_ :: Text -> Html ()
list_ = prop "list"
{-# INLINE list_ #-}

maxlength_ :: Text -> Html ()
maxlength_ = prop "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: Text -> Html ()
minlength_ = prop "minlength"
{-# INLINE minlength_ #-}

method_ :: Text -> Html ()
method_ = prop "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> Html ()
multiple_ = prop "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> Html ()
novalidate_ = prop "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: Text -> Html ()
pattern_ = prop "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> Html ()
readonly_ = prop "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> Html ()
required_ = prop "required"
{-# INLINE required_ #-}

size_ :: Text -> Html ()
size_ = prop "size"
{-# INLINE size_ #-}

forProp_ :: Text -> Html ()
forProp_ = prop "for"
{-# INLINE forProp_ #-}

ref_ :: Text -> Html ()
ref_ = prop "ref"
{-# INLINE ref_ #-}

formProp_ :: Text -> Html ()
formProp_ = prop "form"
{-# INLINE formProp_ #-}

max_ :: Text -> Html ()
max_ = prop "max"
{-# INLINE max_ #-}

min_ :: Text -> Html ()
min_ = prop "min"
{-# INLINE min_ #-}

step_ :: Text -> Html ()
step_ = prop "step"
{-# INLINE step_ #-}

cols_ :: Text -> Html ()
cols_ = prop "cols"
{-# INLINE cols_ #-}

rows_ :: Text -> Html ()
rows_ = prop "rows"
{-# INLINE rows_ #-}

wrap_ :: Text -> Html ()
wrap_ = prop "wrap"
{-# INLINE wrap_ #-}

target_ :: Text -> Html ()
target_ = prop "target"
{-# INLINE target_ #-}

download_ :: Text -> Html ()
download_ = prop "download"
{-# INLINE download_ #-}

downloadAs_ :: Text -> Html ()
downloadAs_ = prop "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: Text -> Html ()
hreflang_ = prop "hreflang"
{-# INLINE hreflang_ #-}

media_ :: Text -> Html ()
media_ = prop "media"
{-# INLINE media_ #-}

ping_ :: Text -> Html ()
ping_ = prop "ping"
{-# INLINE ping_ #-}

rel_ :: Text -> Html ()
rel_ = prop "rel"
{-# INLINE rel_ #-}

ismap_ :: Text -> Html ()
ismap_ = prop "ismap"
{-# INLINE ismap_ #-}

usemap_ :: Text -> Html ()
usemap_ = prop "usemap"
{-# INLINE usemap_ #-}

shape_ :: Text -> Html ()
shape_ = prop "shape"
{-# INLINE shape_ #-}

coords_ :: Text -> Html ()
coords_ = prop "coords"
{-# INLINE coords_ #-}

src_ :: Text -> Html ()
src_ = prop "src"
{-# INLINE src_ #-}

height_ :: Text -> Html ()
height_ = prop "height"
{-# INLINE height_ #-}

width_ :: Text -> Html ()
width_ = prop "width"
{-# INLINE width_ #-}

alt_ :: Text -> Html ()
alt_ = prop "alt"
{-# INLINE alt_ #-}

autoplay_ :: Bool -> Html ()
autoplay_ = prop "autoplay"
{-# INLINE autoplay_ #-}

controls_ :: Bool -> Html ()
controls_ = prop "controls"
{-# INLINE controls_ #-}

loop_ :: Bool -> Html ()
loop_ = prop "loop"
{-# INLINE loop_ #-}

preload_ :: Text -> Html ()
preload_ = prop "preload"
{-# INLINE preload_ #-}

poster_ :: Text -> Html ()
poster_ = prop "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> Html ()
default_ = prop "default"
{-# INLINE default_ #-}

kind_ :: Text -> Html ()
kind_ = prop "kind"
{-# INLINE kind_ #-}

srclang_ :: Text -> Html ()
srclang_ = prop "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: Text -> Html ()
sandbox_ = prop "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: Text -> Html ()
seamless_ = prop "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: Text -> Html ()
srcdoc_ = prop "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: Text -> Html ()
reversed_ = prop "reversed"
{-# INLINE reversed_ #-}

start_ :: Text -> Html ()
start_ = prop "start"
{-# INLINE start_ #-}

align_ :: Text -> Html ()
align_ = prop "align"
{-# INLINE align_ #-}

colspan_ :: Text -> Html ()
colspan_ = attr "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: Text -> Html ()
rowspan_ = attr "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: Text -> Html ()
headers_ = prop "headers"
{-# INLINE headers_ #-}

scope_ :: Text -> Html ()
scope_ = prop "scope"
{-# INLINE scope_ #-}

async_ :: Text -> Html ()
async_ = prop "async"
{-# INLINE async_ #-}

charset_ :: Text -> Html ()
charset_ = prop "charset"
{-# INLINE charset_ #-}

content_ :: Text -> Html ()
content_ = prop "content"
{-# INLINE content_ #-}

defer_ :: Text -> Html ()
defer_ = prop "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: Text -> Html ()
httpEquiv_ = prop "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: Text -> Html ()
language_ = prop "language"
{-# INLINE language_ #-}

scoped_ :: Text -> Html ()
scoped_ = prop "scoped"
{-# INLINE scoped_ #-}

type_ :: Text -> Html ()
type_ = prop "type"
{-# INLINE type_ #-}

name_ :: Text -> Html ()
name_ = prop "name"
{-# INLINE name_ #-}

href_ :: Text -> Html ()
href_ = prop "href"
{-# INLINE href_ #-}

id_ :: Text -> Html ()
id_ = prop "id"
{-# INLINE id_ #-}

placeholder_ :: Text -> Html ()
placeholder_ = prop "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> Html ()
checked_ = prop "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> Html ()
autofocus_ = prop "autofocus"
{-# INLINE autofocus_ #-}

class_ :: Text -> Html ()
class_ = prop "className"
{-# INLINE class_ #-}

data_ :: Text -> Text -> Html ()
data_ k v = prop @Text ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: Text -> Html ()
role_ = attr "role"
{-# INLINE role_ #-}

style_ :: Text -> Html ()
style_ = prop "style"
{-# INLINE style_ #-}
