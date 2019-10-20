{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.Html.Element where

import Massaraksh.Html.Core

-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
div_  = el "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
table_  = el "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
thead_  = el "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
tbody_  = el "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
tr_  = el "tr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
th_  = el "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
td_  = el "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
tfoot_  = el "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
section_  = el "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
header_  = el "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
footer_  = el "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
button_ = el "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
form_ = el "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
p_ = el "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
s_ = el "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ul_ = el "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
span_ = el "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
strong_ = el "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
li_ = el "li"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h1_ = el "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h2_ = el "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h3_ = el "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h4_ = el "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h5_ = el "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h6_ = el "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: [Attribute msg i o] -> Html msg i o
hr_ = flip (el "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
pre_ = el "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: [Attribute msg i o] -> Html msg i o
input_ = flip (el "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
label_ = el "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
a_ = el "a"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
mark_ = el "mark"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ruby_ = el "ruby"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
rt_ = el "rt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
rp_ = el "rp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
bdi_ = el "bdi"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
bdo_ = el "bdo"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: [Attribute msg i o] -> Html msg i o
wbr_ = flip (el "wbr") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
details_ = el "details"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
summary_ = el "summary"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
menuitem_ = el "menuitem"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
menu_ = el "menu"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
fieldset_ = el "fieldset"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
legend_ = el "legend"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
datalist_ = el "datalist"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
optgroup_ = el "optgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
keygen_ = el "keygen"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
output_ = el "output"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
progress_ = el "progress"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
meter_ = el "meter"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
center_ = el "center"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
audio_ = el "audio"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
video_ = el "video"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: [Attribute msg i o] -> Html msg i o
source_ = flip (el "source") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: [Attribute msg i o] -> Html msg i o
track_ = flip (el "track") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: [Attribute msg i o] -> Html msg i o
embed_ = flip (el "embed") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
object_ = el "object"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: [Attribute msg i o] -> Html msg i o
param_ = flip (el "param") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ins_ = el "ins"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
del_ = el "del"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
small_ = el "small"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
cite_ = el "cite"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dfn_ = el "dfn"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
abbr_ = el "abbr"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
time_ = el "time"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
var_ = el "var"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
samp_ = el "samp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
kbd_ = el "kbd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
caption_ = el "caption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
colgroup_ = el "colgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: [Attribute msg i o] -> Html msg i o
col_ = flip (el "col") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
nav_ = el "nav"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
article_ = el "article"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
aside_ = el "aside"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
address_ = el "address"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
main_ = el "main"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
body_ = el "body"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
figure_ = el "figure"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
figcaption_ = el "figcaption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dl_ = el "dl"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dt_ = el "dt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dd_ = el "dd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: [Attribute msg i o] -> Html msg i o
img_ = flip (el "img") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
iframe_ = el "iframe"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
canvas_ = el "canvas"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
math_ = el "math"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
select_ = el "select"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
option_ = el "option"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
textarea_ = el "textarea"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
sub_ = el "sub"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
sup_ = el "sup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: [Attribute msg i o] -> Html msg i o
br_ = flip (el "br") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ol_ = el "ol"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
blockquote_ = el "blockquote"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
code_ = el "code"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
em_ = el "em"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
i_ = el "i"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
b_ = el "b"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
u_ = el "u"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
q_ = el "q"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
script_ = el "script"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: [Attribute msg i o] -> Html msg i o
link_ = flip (el "link") []
