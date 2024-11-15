{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-|
Shortcuts for common HTML5 attributes and properties
-}
module Clickable.Property where

import Clickable.HTML
import Clickable.Types
import Data.Text


-- TODO: Real-world usage has demonstrated that 'dynStyles' not
-- sufficiently composable. For instance, if 'dynStyles' is used to
-- set the CSS color for an element, essentially no other CSS property
-- can be applied to this element, as they will be overwritten by
-- 'dynStyles'.
dynStyles :: Dynamic Text -> HTML ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: Dynamic Text -> HTML ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: Dynamic Text -> HTML ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: Dynamic Bool -> HTML ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: Dynamic Bool -> HTML ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}

title_ :: Text -> HTML ()
title_ = property "title"
{-# INLINE title_ #-}

selected_ :: Bool -> HTML ()
selected_ = property "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> HTML ()
hidden_ = property "hidden"
{-# INLINE hidden_ #-}

value_ :: Text -> HTML ()
value_ = property "value"
{-# INLINE value_ #-}

defaultValue_ :: Text -> HTML ()
defaultValue_ = property "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: Text -> HTML ()
accept_ = property "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: Text -> HTML ()
acceptCharset_ = property "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: Text -> HTML ()
action_ = property "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> HTML ()
autocomplete_ b = property @Text "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: Text -> HTML ()
autosave_ = property "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> HTML ()
disabled_ = property "disabled"
{-# INLINE disabled_ #-}

enctype_ :: Text -> HTML ()
enctype_ = property "enctype"
{-# INLINE enctype_ #-}

formation_ :: Text -> HTML ()
formation_ = property "formation"
{-# INLINE formation_ #-}

list_ :: Text -> HTML ()
list_ = property "list"
{-# INLINE list_ #-}

maxlength_ :: Text -> HTML ()
maxlength_ = property "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: Text -> HTML ()
minlength_ = property "minlength"
{-# INLINE minlength_ #-}

method_ :: Text -> HTML ()
method_ = property "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> HTML ()
multiple_ = property "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> HTML ()
novalidate_ = property "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: Text -> HTML ()
pattern_ = property "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> HTML ()
readonly_ = property "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> HTML ()
required_ = property "required"
{-# INLINE required_ #-}

size_ :: Text -> HTML ()
size_ = property "size"
{-# INLINE size_ #-}

forProp_ :: Text -> HTML ()
forProp_ = property "for"
{-# INLINE forProp_ #-}

ref_ :: Text -> HTML ()
ref_ = property "ref"
{-# INLINE ref_ #-}

formProp_ :: Text -> HTML ()
formProp_ = property "form"
{-# INLINE formProp_ #-}

max_ :: Text -> HTML ()
max_ = property "max"
{-# INLINE max_ #-}

min_ :: Text -> HTML ()
min_ = property "min"
{-# INLINE min_ #-}

step_ :: Text -> HTML ()
step_ = property "step"
{-# INLINE step_ #-}

cols_ :: Text -> HTML ()
cols_ = property "cols"
{-# INLINE cols_ #-}

rows_ :: Text -> HTML ()
rows_ = property "rows"
{-# INLINE rows_ #-}

wrap_ :: Text -> HTML ()
wrap_ = property "wrap"
{-# INLINE wrap_ #-}

target_ :: Text -> HTML ()
target_ = property "target"
{-# INLINE target_ #-}

download_ :: Text -> HTML ()
download_ = property "download"
{-# INLINE download_ #-}

downloadAs_ :: Text -> HTML ()
downloadAs_ = property "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: Text -> HTML ()
hreflang_ = property "hreflang"
{-# INLINE hreflang_ #-}

media_ :: Text -> HTML ()
media_ = property "media"
{-# INLINE media_ #-}

ping_ :: Text -> HTML ()
ping_ = property "ping"
{-# INLINE ping_ #-}

rel_ :: Text -> HTML ()
rel_ = property "rel"
{-# INLINE rel_ #-}

ismap_ :: Text -> HTML ()
ismap_ = property "ismap"
{-# INLINE ismap_ #-}

usemap_ :: Text -> HTML ()
usemap_ = property "usemap"
{-# INLINE usemap_ #-}

shape_ :: Text -> HTML ()
shape_ = property "shape"
{-# INLINE shape_ #-}

coords_ :: Text -> HTML ()
coords_ = property "coords"
{-# INLINE coords_ #-}

src_ :: Text -> HTML ()
src_ = property "src"
{-# INLINE src_ #-}

height_ :: Text -> HTML ()
height_ = property "height"
{-# INLINE height_ #-}

width_ :: Text -> HTML ()
width_ = property "width"
{-# INLINE width_ #-}

alt_ :: Text -> HTML ()
alt_ = property "alt"
{-# INLINE alt_ #-}

autoplay_ :: Bool -> HTML ()
autoplay_ = property "autoplay"
{-# INLINE autoplay_ #-}

controls_ :: Bool -> HTML ()
controls_ = property "controls"
{-# INLINE controls_ #-}

loop_ :: Bool -> HTML ()
loop_ = property "loop"
{-# INLINE loop_ #-}

preload_ :: Text -> HTML ()
preload_ = property "preload"
{-# INLINE preload_ #-}

poster_ :: Text -> HTML ()
poster_ = property "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> HTML ()
default_ = property "default"
{-# INLINE default_ #-}

kind_ :: Text -> HTML ()
kind_ = property "kind"
{-# INLINE kind_ #-}

srclang_ :: Text -> HTML ()
srclang_ = property "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: Text -> HTML ()
sandbox_ = property "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: Text -> HTML ()
seamless_ = property "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: Text -> HTML ()
srcdoc_ = property "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: Text -> HTML ()
reversed_ = property "reversed"
{-# INLINE reversed_ #-}

start_ :: Text -> HTML ()
start_ = property "start"
{-# INLINE start_ #-}

align_ :: Text -> HTML ()
align_ = property "align"
{-# INLINE align_ #-}

colspan_ :: Text -> HTML ()
colspan_ = attribute "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: Text -> HTML ()
rowspan_ = attribute "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: Text -> HTML ()
headers_ = property "headers"
{-# INLINE headers_ #-}

scope_ :: Text -> HTML ()
scope_ = property "scope"
{-# INLINE scope_ #-}

async_ :: Text -> HTML ()
async_ = property "async"
{-# INLINE async_ #-}

charset_ :: Text -> HTML ()
charset_ = property "charset"
{-# INLINE charset_ #-}

content_ :: Text -> HTML ()
content_ = property "content"
{-# INLINE content_ #-}

defer_ :: Text -> HTML ()
defer_ = property "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: Text -> HTML ()
httpEquiv_ = property "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: Text -> HTML ()
language_ = property "language"
{-# INLINE language_ #-}

scoped_ :: Text -> HTML ()
scoped_ = property "scoped"
{-# INLINE scoped_ #-}

type_ :: Text -> HTML ()
type_ = property "type"
{-# INLINE type_ #-}

name_ :: Text -> HTML ()
name_ = property "name"
{-# INLINE name_ #-}

href_ :: Text -> HTML ()
href_ = property "href"
{-# INLINE href_ #-}

id_ :: Text -> HTML ()
id_ = property "id"
{-# INLINE id_ #-}

placeholder_ :: Text -> HTML ()
placeholder_ = property "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> HTML ()
checked_ = property "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> HTML ()
autofocus_ = property "autofocus"
{-# INLINE autofocus_ #-}

class_ :: Text -> HTML ()
class_ = property "className"
{-# INLINE class_ #-}

data_ :: Text -> Text -> HTML ()
data_ k v = property ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: Text -> HTML ()
role_ = attribute "role"
{-# INLINE role_ #-}

style_ :: Text -> HTML ()
style_ = property "style"
{-# INLINE style_ #-}
