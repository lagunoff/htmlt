{-|
Shortcuts for common HTML5 attributes and properties
-}
module HtmlT.Property where

import HtmlT.Base
import HtmlT.Event
import HtmlT.Types
import Wasm.Compat.Prim


-- TODO: Real-world usage has demonstrated that 'dynStyles' not
-- sufficiently composable. For instance, if 'dynStyles' is used to
-- set the CSS color for an element, essentially no other CSS property
-- can be applied to this element, as they will be overwritten by
-- 'dynStyles'.
dynStyles :: Dynamic JSString -> Html ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: Dynamic JSString -> Html ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: Dynamic JSString -> Html ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: Dynamic Bool -> Html ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: Dynamic Bool -> Html ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}

title_ :: JSString -> Html ()
title_ = prop "title"
{-# INLINE title_ #-}

selected_ :: Bool -> Html ()
selected_ = prop "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> Html ()
hidden_ = prop "hidden"
{-# INLINE hidden_ #-}

value_ :: JSString -> Html ()
value_ = prop "value"
{-# INLINE value_ #-}

defaultValue_ :: JSString -> Html ()
defaultValue_ = prop "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: JSString -> Html ()
accept_ = prop "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: JSString -> Html ()
acceptCharset_ = prop "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: JSString -> Html ()
action_ = prop "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> Html ()
autocomplete_ b = prop @JSString "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: JSString -> Html ()
autosave_ = prop "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> Html ()
disabled_ = prop "disabled"
{-# INLINE disabled_ #-}

enctype_ :: JSString -> Html ()
enctype_ = prop "enctype"
{-# INLINE enctype_ #-}

formation_ :: JSString -> Html ()
formation_ = prop "formation"
{-# INLINE formation_ #-}

list_ :: JSString -> Html ()
list_ = prop "list"
{-# INLINE list_ #-}

maxlength_ :: JSString -> Html ()
maxlength_ = prop "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: JSString -> Html ()
minlength_ = prop "minlength"
{-# INLINE minlength_ #-}

method_ :: JSString -> Html ()
method_ = prop "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> Html ()
multiple_ = prop "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> Html ()
novalidate_ = prop "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: JSString -> Html ()
pattern_ = prop "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> Html ()
readonly_ = prop "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> Html ()
required_ = prop "required"
{-# INLINE required_ #-}

size_ :: JSString -> Html ()
size_ = prop "size"
{-# INLINE size_ #-}

forProp_ :: JSString -> Html ()
forProp_ = prop "for"
{-# INLINE forProp_ #-}

ref_ :: JSString -> Html ()
ref_ = prop "ref"
{-# INLINE ref_ #-}

formProp_ :: JSString -> Html ()
formProp_ = prop "form"
{-# INLINE formProp_ #-}

max_ :: JSString -> Html ()
max_ = prop "max"
{-# INLINE max_ #-}

min_ :: JSString -> Html ()
min_ = prop "min"
{-# INLINE min_ #-}

step_ :: JSString -> Html ()
step_ = prop "step"
{-# INLINE step_ #-}

cols_ :: JSString -> Html ()
cols_ = prop "cols"
{-# INLINE cols_ #-}

rows_ :: JSString -> Html ()
rows_ = prop "rows"
{-# INLINE rows_ #-}

wrap_ :: JSString -> Html ()
wrap_ = prop "wrap"
{-# INLINE wrap_ #-}

target_ :: JSString -> Html ()
target_ = prop "target"
{-# INLINE target_ #-}

download_ :: JSString -> Html ()
download_ = prop "download"
{-# INLINE download_ #-}

downloadAs_ :: JSString -> Html ()
downloadAs_ = prop "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: JSString -> Html ()
hreflang_ = prop "hreflang"
{-# INLINE hreflang_ #-}

media_ :: JSString -> Html ()
media_ = prop "media"
{-# INLINE media_ #-}

ping_ :: JSString -> Html ()
ping_ = prop "ping"
{-# INLINE ping_ #-}

rel_ :: JSString -> Html ()
rel_ = prop "rel"
{-# INLINE rel_ #-}

ismap_ :: JSString -> Html ()
ismap_ = prop "ismap"
{-# INLINE ismap_ #-}

usemap_ :: JSString -> Html ()
usemap_ = prop "usemap"
{-# INLINE usemap_ #-}

shape_ :: JSString -> Html ()
shape_ = prop "shape"
{-# INLINE shape_ #-}

coords_ :: JSString -> Html ()
coords_ = prop "coords"
{-# INLINE coords_ #-}

src_ :: JSString -> Html ()
src_ = prop "src"
{-# INLINE src_ #-}

height_ :: JSString -> Html ()
height_ = prop "height"
{-# INLINE height_ #-}

width_ :: JSString -> Html ()
width_ = prop "width"
{-# INLINE width_ #-}

alt_ :: JSString -> Html ()
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

preload_ :: JSString -> Html ()
preload_ = prop "preload"
{-# INLINE preload_ #-}

poster_ :: JSString -> Html ()
poster_ = prop "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> Html ()
default_ = prop "default"
{-# INLINE default_ #-}

kind_ :: JSString -> Html ()
kind_ = prop "kind"
{-# INLINE kind_ #-}

srclang_ :: JSString -> Html ()
srclang_ = prop "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: JSString -> Html ()
sandbox_ = prop "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: JSString -> Html ()
seamless_ = prop "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: JSString -> Html ()
srcdoc_ = prop "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: JSString -> Html ()
reversed_ = prop "reversed"
{-# INLINE reversed_ #-}

start_ :: JSString -> Html ()
start_ = prop "start"
{-# INLINE start_ #-}

align_ :: JSString -> Html ()
align_ = prop "align"
{-# INLINE align_ #-}

colspan_ :: JSString -> Html ()
colspan_ = attr "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: JSString -> Html ()
rowspan_ = attr "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: JSString -> Html ()
headers_ = prop "headers"
{-# INLINE headers_ #-}

scope_ :: JSString -> Html ()
scope_ = prop "scope"
{-# INLINE scope_ #-}

async_ :: JSString -> Html ()
async_ = prop "async"
{-# INLINE async_ #-}

charset_ :: JSString -> Html ()
charset_ = prop "charset"
{-# INLINE charset_ #-}

content_ :: JSString -> Html ()
content_ = prop "content"
{-# INLINE content_ #-}

defer_ :: JSString -> Html ()
defer_ = prop "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: JSString -> Html ()
httpEquiv_ = prop "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: JSString -> Html ()
language_ = prop "language"
{-# INLINE language_ #-}

scoped_ :: JSString -> Html ()
scoped_ = prop "scoped"
{-# INLINE scoped_ #-}

type_ :: JSString -> Html ()
type_ = prop "type"
{-# INLINE type_ #-}

name_ :: JSString -> Html ()
name_ = prop "name"
{-# INLINE name_ #-}

href_ :: JSString -> Html ()
href_ = prop "href"
{-# INLINE href_ #-}

id_ :: JSString -> Html ()
id_ = prop "id"
{-# INLINE id_ #-}

placeholder_ :: JSString -> Html ()
placeholder_ = prop "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> Html ()
checked_ = prop "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> Html ()
autofocus_ = prop "autofocus"
{-# INLINE autofocus_ #-}

class_ :: JSString -> Html ()
class_ = prop "className"
{-# INLINE class_ #-}

data_ :: JSString -> JSString -> Html ()
data_ k v = prop @JSString ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: JSString -> Html ()
role_ = attr "role"
{-# INLINE role_ #-}

style_ :: JSString -> Html ()
style_ = prop "style"
{-# INLINE style_ #-}
