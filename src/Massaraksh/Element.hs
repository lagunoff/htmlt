{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Element@
-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
module Massaraksh.Element where

import Massaraksh.Base

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: HasDom e => HtmlM e a -> HtmlM e a
div_ = el "div"
{-# INLINE div_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: HasDom e => HtmlM e a -> HtmlM e a
table_ = el "table"
{-# INLINE table_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: HasDom e => HtmlM e a -> HtmlM e a
thead_ = el "thead"
{-# INLINE thead_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: HasDom e => HtmlM e a -> HtmlM e a
tbody_ = el "tbody"
{-# INLINE tbody_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: HasDom e => HtmlM e a -> HtmlM e a
tr_ = el "tr"
{-# INLINE tr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: HasDom e => HtmlM e a -> HtmlM e a
th_ = el "th"
{-# INLINE th_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: HasDom e => HtmlM e a -> HtmlM e a
td_ = el "td"
{-# INLINE td_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: HasDom e => HtmlM e a -> HtmlM e a
tfoot_ = el "tfoot"
{-# INLINE tfoot_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: HasDom e => HtmlM e a -> HtmlM e a
section_ = el "section"
{-# INLINE section_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: HasDom e => HtmlM e a -> HtmlM e a
header_ = el "header"
{-# INLINE header_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: HasDom e => HtmlM e a -> HtmlM e a
footer_ = el "footer"
{-# INLINE footer_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: HasDom e => HtmlM e a -> HtmlM e a
button_ = el "button"
{-# INLINE button_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: HasDom e => HtmlM e a -> HtmlM e a
form_ = el "form"
{-# INLINE form_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: HasDom e => HtmlM e a -> HtmlM e a
p_ = el "p"
{-# INLINE p_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: HasDom e => HtmlM e a -> HtmlM e a
s_ = el "s"
{-# INLINE s_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: HasDom e => HtmlM e a -> HtmlM e a
ul_ = el "ul"
{-# INLINE ul_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: HasDom e => HtmlM e a -> HtmlM e a
span_ = el "span"
{-# INLINE span_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: HasDom e => HtmlM e a -> HtmlM e a
strong_ = el "strong"
{-# INLINE strong_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: HasDom e => HtmlM e a -> HtmlM e a
li_ = el "li"
{-# INLINE li_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: HasDom e => HtmlM e a -> HtmlM e a
h1_ = el "h1"
{-# INLINE h1_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: HasDom e => HtmlM e a -> HtmlM e a
h2_ = el "h2"
{-# INLINE h2_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: HasDom e => HtmlM e a -> HtmlM e a
h3_ = el "h3"
{-# INLINE h3_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: HasDom e => HtmlM e a -> HtmlM e a
h4_ = el "h4"
{-# INLINE h4_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: HasDom e => HtmlM e a -> HtmlM e a
h5_ = el "h5"
{-# INLINE h5_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: HasDom e => HtmlM e a -> HtmlM e a
h6_ = el "h6"
{-# INLINE h6_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: HasDom e => HtmlM e a -> HtmlM e a
hr_ = el "hr"
{-# INLINE hr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: HasDom e => HtmlM e a -> HtmlM e a
pre_ = el "pre"
{-# INLINE pre_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: HasDom e => HtmlM e a -> HtmlM e a
input_ = el "input"
{-# INLINE input_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: HasDom e => HtmlM e a -> HtmlM e a
label_ = el "label"
{-# INLINE label_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: HasDom e => HtmlM e a -> HtmlM e a
a_ = el "a"
{-# INLINE a_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: HasDom e => HtmlM e a -> HtmlM e a
mark_ = el "mark"
{-# INLINE mark_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: HasDom e => HtmlM e a -> HtmlM e a
ruby_ = el "ruby"
{-# INLINE ruby_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: HasDom e => HtmlM e a -> HtmlM e a
rt_ = el "rt"
{-# INLINE rt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: HasDom e => HtmlM e a -> HtmlM e a
rp_ = el "rp"
{-# INLINE rp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: HasDom e => HtmlM e a -> HtmlM e a
bdi_ = el "bdi"
{-# INLINE bdi_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: HasDom e => HtmlM e a -> HtmlM e a
bdo_ = el "bdo"
{-# INLINE bdo_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: HasDom e => HtmlM e a -> HtmlM e a
wbr_ = el "wbr"
{-# INLINE wbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: HasDom e => HtmlM e a -> HtmlM e a
details_ = el "details"
{-# INLINE details_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: HasDom e => HtmlM e a -> HtmlM e a
summary_ = el "summary"
{-# INLINE summary_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: HasDom e => HtmlM e a -> HtmlM e a
menuitem_ = el "menuitem"
{-# INLINE menuitem_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: HasDom e => HtmlM e a -> HtmlM e a
menu_ = el "menu"
{-# INLINE menu_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: HasDom e => HtmlM e a -> HtmlM e a
fieldset_ = el "fieldset"
{-# INLINE fieldset_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: HasDom e => HtmlM e a -> HtmlM e a
legend_ = el "legend"
{-# INLINE legend_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: HasDom e => HtmlM e a -> HtmlM e a
datalist_ = el "datalist"
{-# INLINE datalist_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: HasDom e => HtmlM e a -> HtmlM e a
optgroup_ = el "optgroup"
{-# INLINE optgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: HasDom e => HtmlM e a -> HtmlM e a
keygen_ = el "keygen"
{-# INLINE keygen_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: HasDom e => HtmlM e a -> HtmlM e a
output_ = el "output"
{-# INLINE output_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: HasDom e => HtmlM e a -> HtmlM e a
progress_ = el "progress"
{-# INLINE progress_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: HasDom e => HtmlM e a -> HtmlM e a
meter_ = el "meter"
{-# INLINE meter_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: HasDom e => HtmlM e a -> HtmlM e a
center_ = el "center"
{-# INLINE center_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: HasDom e => HtmlM e a -> HtmlM e a
audio_ = el "audio"
{-# INLINE audio_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: HasDom e => HtmlM e a -> HtmlM e a
video_ = el "video"
{-# INLINE video_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: HasDom e => HtmlM e a -> HtmlM e a
source_ = el "source"
{-# INLINE source_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: HasDom e => HtmlM e a -> HtmlM e a
track_ = el "track"
{-# INLINE track_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: HasDom e => HtmlM e a -> HtmlM e a
embed_ = el "embed"
{-# INLINE embed_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: HasDom e => HtmlM e a -> HtmlM e a
object_ = el "object"
{-# INLINE object_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: HasDom e => HtmlM e a -> HtmlM e a
param_ = el "param"
{-# INLINE param_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: HasDom e => HtmlM e a -> HtmlM e a
ins_ = el "ins"
{-# INLINE ins_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: HasDom e => HtmlM e a -> HtmlM e a
del_ = el "del"
{-# INLINE del_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: HasDom e => HtmlM e a -> HtmlM e a
small_ = el "small"
{-# INLINE small_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: HasDom e => HtmlM e a -> HtmlM e a
cite_ = el "cite"
{-# INLINE cite_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: HasDom e => HtmlM e a -> HtmlM e a
dfn_ = el "dfn"
{-# INLINE dfn_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: HasDom e => HtmlM e a -> HtmlM e a
abbr_ = el "abbr"
{-# INLINE abbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: HasDom e => HtmlM e a -> HtmlM e a
time_ = el "time"
{-# INLINE time_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: HasDom e => HtmlM e a -> HtmlM e a
var_ = el "var"
{-# INLINE var_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: HasDom e => HtmlM e a -> HtmlM e a
samp_ = el "samp"
{-# INLINE samp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: HasDom e => HtmlM e a -> HtmlM e a
kbd_ = el "kbd"
{-# INLINE kbd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: HasDom e => HtmlM e a -> HtmlM e a
caption_ = el "caption"
{-# INLINE caption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: HasDom e => HtmlM e a -> HtmlM e a
colgroup_ = el "colgroup"
{-# INLINE colgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: HasDom e => HtmlM e a -> HtmlM e a
col_ = el "col"
{-# INLINE col_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: HasDom e => HtmlM e a -> HtmlM e a
nav_ = el "nav"
{-# INLINE nav_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: HasDom e => HtmlM e a -> HtmlM e a
article_ = el "article"
{-# INLINE article_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: HasDom e => HtmlM e a -> HtmlM e a
aside_ = el "aside"
{-# INLINE aside_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: HasDom e => HtmlM e a -> HtmlM e a
address_ = el "address"
{-# INLINE address_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: HasDom e => HtmlM e a -> HtmlM e a
main_ = el "main"
{-# INLINE main_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: HasDom e => HtmlM e a -> HtmlM e a
body_ = el "body"
{-# INLINE body_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: HasDom e => HtmlM e a -> HtmlM e a
figure_ = el "figure"
{-# INLINE figure_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: HasDom e => HtmlM e a -> HtmlM e a
figcaption_ = el "figcaption"
{-# INLINE figcaption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: HasDom e => HtmlM e a -> HtmlM e a
dl_ = el "dl"
{-# INLINE dl_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: HasDom e => HtmlM e a -> HtmlM e a
dt_ = el "dt"
{-# INLINE dt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: HasDom e => HtmlM e a -> HtmlM e a
dd_ = el "dd"
{-# INLINE dd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: HasDom e => HtmlM e a -> HtmlM e a
img_ = el "img"
{-# INLINE img_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: HasDom e => HtmlM e a -> HtmlM e a
iframe_ = el "iframe"
{-# INLINE iframe_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: HasDom e => HtmlM e a -> HtmlM e a
canvas_ = el "canvas"
{-# INLINE canvas_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: HasDom e => HtmlM e a -> HtmlM e a
math_ = el "math"
{-# INLINE math_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: HasDom e => HtmlM e a -> HtmlM e a
select_ = el "select"
{-# INLINE select_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: HasDom e => HtmlM e a -> HtmlM e a
option_ = el "option"
{-# INLINE option_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: HasDom e => HtmlM e a -> HtmlM e a
textarea_ = el "textarea"
{-# INLINE textarea_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: HasDom e => HtmlM e a -> HtmlM e a
sub_ = el "sub"
{-# INLINE sub_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: HasDom e => HtmlM e a -> HtmlM e a
sup_ = el "sup"
{-# INLINE sup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: HasDom e => HtmlM e a -> HtmlM e a
br_ = el "br"
{-# INLINE br_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: HasDom e => HtmlM e a -> HtmlM e a
ol_ = el "ol"
{-# INLINE ol_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: HasDom e => HtmlM e a -> HtmlM e a
blockquote_ = el "blockquote"
{-# INLINE blockquote_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: HasDom e => HtmlM e a -> HtmlM e a
code_ = el "code"
{-# INLINE code_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: HasDom e => HtmlM e a -> HtmlM e a
em_ = el "em"
{-# INLINE em_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: HasDom e => HtmlM e a -> HtmlM e a
i_ = el "i"
{-# INLINE i_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: HasDom e => HtmlM e a -> HtmlM e a
b_ = el "b"
{-# INLINE b_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: HasDom e => HtmlM e a -> HtmlM e a
u_ = el "u"
{-# INLINE u_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: HasDom e => HtmlM e a -> HtmlM e a
q_ = el "q"
{-# INLINE q_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: HasDom e => HtmlM e a -> HtmlM e a
script_ = el "script"
{-# INLINE script_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: HasDom e => HtmlM e a -> HtmlM e a
link_ = el "link"
{-# INLINE link_ #-}
