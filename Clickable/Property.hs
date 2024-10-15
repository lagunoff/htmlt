{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-|
Shortcuts for common HTML5 attributes and properties
-}
module Clickable.Property where

import Clickable.Html
import Clickable.Types
import Data.Text


-- TODO: Real-world usage has demonstrated that 'dynStyles' not
-- sufficiently composable. For instance, if 'dynStyles' is used to
-- set the CSS color for an element, essentially no other CSS property
-- can be applied to this element, as they will be overwritten by
-- 'dynStyles'.
dynStyles :: DynVal Text -> HtmlM ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: DynVal Text -> HtmlM ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: DynVal Text -> HtmlM ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: DynVal Bool -> HtmlM ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: DynVal Bool -> HtmlM ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}

title_ :: Text -> HtmlM ()
title_ = property "title"
{-# INLINE title_ #-}

selected_ :: Bool -> HtmlM ()
selected_ = property "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> HtmlM ()
hidden_ = property "hidden"
{-# INLINE hidden_ #-}

value_ :: Text -> HtmlM ()
value_ = property "value"
{-# INLINE value_ #-}

defaultValue_ :: Text -> HtmlM ()
defaultValue_ = property "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: Text -> HtmlM ()
accept_ = property "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: Text -> HtmlM ()
acceptCharset_ = property "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: Text -> HtmlM ()
action_ = property "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> HtmlM ()
autocomplete_ b = property @Text "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: Text -> HtmlM ()
autosave_ = property "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> HtmlM ()
disabled_ = property "disabled"
{-# INLINE disabled_ #-}

enctype_ :: Text -> HtmlM ()
enctype_ = property "enctype"
{-# INLINE enctype_ #-}

formation_ :: Text -> HtmlM ()
formation_ = property "formation"
{-# INLINE formation_ #-}

list_ :: Text -> HtmlM ()
list_ = property "list"
{-# INLINE list_ #-}

maxlength_ :: Text -> HtmlM ()
maxlength_ = property "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: Text -> HtmlM ()
minlength_ = property "minlength"
{-# INLINE minlength_ #-}

method_ :: Text -> HtmlM ()
method_ = property "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> HtmlM ()
multiple_ = property "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> HtmlM ()
novalidate_ = property "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: Text -> HtmlM ()
pattern_ = property "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> HtmlM ()
readonly_ = property "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> HtmlM ()
required_ = property "required"
{-# INLINE required_ #-}

size_ :: Text -> HtmlM ()
size_ = property "size"
{-# INLINE size_ #-}

forProp_ :: Text -> HtmlM ()
forProp_ = property "for"
{-# INLINE forProp_ #-}

ref_ :: Text -> HtmlM ()
ref_ = property "ref"
{-# INLINE ref_ #-}

formProp_ :: Text -> HtmlM ()
formProp_ = property "form"
{-# INLINE formProp_ #-}

max_ :: Text -> HtmlM ()
max_ = property "max"
{-# INLINE max_ #-}

min_ :: Text -> HtmlM ()
min_ = property "min"
{-# INLINE min_ #-}

step_ :: Text -> HtmlM ()
step_ = property "step"
{-# INLINE step_ #-}

cols_ :: Text -> HtmlM ()
cols_ = property "cols"
{-# INLINE cols_ #-}

rows_ :: Text -> HtmlM ()
rows_ = property "rows"
{-# INLINE rows_ #-}

wrap_ :: Text -> HtmlM ()
wrap_ = property "wrap"
{-# INLINE wrap_ #-}

target_ :: Text -> HtmlM ()
target_ = property "target"
{-# INLINE target_ #-}

download_ :: Text -> HtmlM ()
download_ = property "download"
{-# INLINE download_ #-}

downloadAs_ :: Text -> HtmlM ()
downloadAs_ = property "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: Text -> HtmlM ()
hreflang_ = property "hreflang"
{-# INLINE hreflang_ #-}

media_ :: Text -> HtmlM ()
media_ = property "media"
{-# INLINE media_ #-}

ping_ :: Text -> HtmlM ()
ping_ = property "ping"
{-# INLINE ping_ #-}

rel_ :: Text -> HtmlM ()
rel_ = property "rel"
{-# INLINE rel_ #-}

ismap_ :: Text -> HtmlM ()
ismap_ = property "ismap"
{-# INLINE ismap_ #-}

usemap_ :: Text -> HtmlM ()
usemap_ = property "usemap"
{-# INLINE usemap_ #-}

shape_ :: Text -> HtmlM ()
shape_ = property "shape"
{-# INLINE shape_ #-}

coords_ :: Text -> HtmlM ()
coords_ = property "coords"
{-# INLINE coords_ #-}

src_ :: Text -> HtmlM ()
src_ = property "src"
{-# INLINE src_ #-}

height_ :: Text -> HtmlM ()
height_ = property "height"
{-# INLINE height_ #-}

width_ :: Text -> HtmlM ()
width_ = property "width"
{-# INLINE width_ #-}

alt_ :: Text -> HtmlM ()
alt_ = property "alt"
{-# INLINE alt_ #-}

autoplay_ :: Bool -> HtmlM ()
autoplay_ = property "autoplay"
{-# INLINE autoplay_ #-}

controls_ :: Bool -> HtmlM ()
controls_ = property "controls"
{-# INLINE controls_ #-}

loop_ :: Bool -> HtmlM ()
loop_ = property "loop"
{-# INLINE loop_ #-}

preload_ :: Text -> HtmlM ()
preload_ = property "preload"
{-# INLINE preload_ #-}

poster_ :: Text -> HtmlM ()
poster_ = property "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> HtmlM ()
default_ = property "default"
{-# INLINE default_ #-}

kind_ :: Text -> HtmlM ()
kind_ = property "kind"
{-# INLINE kind_ #-}

srclang_ :: Text -> HtmlM ()
srclang_ = property "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: Text -> HtmlM ()
sandbox_ = property "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: Text -> HtmlM ()
seamless_ = property "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: Text -> HtmlM ()
srcdoc_ = property "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: Text -> HtmlM ()
reversed_ = property "reversed"
{-# INLINE reversed_ #-}

start_ :: Text -> HtmlM ()
start_ = property "start"
{-# INLINE start_ #-}

align_ :: Text -> HtmlM ()
align_ = property "align"
{-# INLINE align_ #-}

colspan_ :: Text -> HtmlM ()
colspan_ = attribute "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: Text -> HtmlM ()
rowspan_ = attribute "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: Text -> HtmlM ()
headers_ = property "headers"
{-# INLINE headers_ #-}

scope_ :: Text -> HtmlM ()
scope_ = property "scope"
{-# INLINE scope_ #-}

async_ :: Text -> HtmlM ()
async_ = property "async"
{-# INLINE async_ #-}

charset_ :: Text -> HtmlM ()
charset_ = property "charset"
{-# INLINE charset_ #-}

content_ :: Text -> HtmlM ()
content_ = property "content"
{-# INLINE content_ #-}

defer_ :: Text -> HtmlM ()
defer_ = property "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: Text -> HtmlM ()
httpEquiv_ = property "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: Text -> HtmlM ()
language_ = property "language"
{-# INLINE language_ #-}

scoped_ :: Text -> HtmlM ()
scoped_ = property "scoped"
{-# INLINE scoped_ #-}

type_ :: Text -> HtmlM ()
type_ = property "type"
{-# INLINE type_ #-}

name_ :: Text -> HtmlM ()
name_ = property "name"
{-# INLINE name_ #-}

href_ :: Text -> HtmlM ()
href_ = property "href"
{-# INLINE href_ #-}

id_ :: Text -> HtmlM ()
id_ = property "id"
{-# INLINE id_ #-}

placeholder_ :: Text -> HtmlM ()
placeholder_ = property "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> HtmlM ()
checked_ = property "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> HtmlM ()
autofocus_ = property "autofocus"
{-# INLINE autofocus_ #-}

class_ :: Text -> HtmlM ()
class_ = property "className"
{-# INLINE class_ #-}

data_ :: Text -> Text -> HtmlM ()
data_ k v = property ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: Text -> HtmlM ()
role_ = attribute "role"
{-# INLINE role_ #-}

style_ :: Text -> HtmlM ()
style_ = property "style"
{-# INLINE style_ #-}
