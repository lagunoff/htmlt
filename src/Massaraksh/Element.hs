{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Element@
-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
module Massaraksh.Element where

import Massaraksh.Types
import Massaraksh.Base

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: HtmlBase m => HtmlT m x -> HtmlT m x
div_ = el "div"
{-# INLINE div_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: HtmlBase m => HtmlT m x -> HtmlT m x
table_ = el "table"
{-# INLINE table_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: HtmlBase m => HtmlT m x -> HtmlT m x
thead_ = el "thead"
{-# INLINE thead_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: HtmlBase m => HtmlT m x -> HtmlT m x
tbody_ = el "tbody"
{-# INLINE tbody_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
tr_ = el "tr"
{-# INLINE tr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: HtmlBase m => HtmlT m x -> HtmlT m x
th_ = el "th"
{-# INLINE th_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: HtmlBase m => HtmlT m x -> HtmlT m x
td_ = el "td"
{-# INLINE td_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: HtmlBase m => HtmlT m x -> HtmlT m x
tfoot_ = el "tfoot"
{-# INLINE tfoot_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: HtmlBase m => HtmlT m x -> HtmlT m x
section_ = el "section"
{-# INLINE section_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: HtmlBase m => HtmlT m x -> HtmlT m x
header_ = el "header"
{-# INLINE header_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: HtmlBase m => HtmlT m x -> HtmlT m x
footer_ = el "footer"
{-# INLINE footer_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: HtmlBase m => HtmlT m x -> HtmlT m x
button_ = el "button"
{-# INLINE button_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: HtmlBase m => HtmlT m x -> HtmlT m x
form_ = el "form"
{-# INLINE form_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: HtmlBase m => HtmlT m x -> HtmlT m x
p_ = el "p"
{-# INLINE p_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: HtmlBase m => HtmlT m x -> HtmlT m x
s_ = el "s"
{-# INLINE s_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ul_ = el "ul"
{-# INLINE ul_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: HtmlBase m => HtmlT m x -> HtmlT m x
span_ = el "span"
{-# INLINE span_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: HtmlBase m => HtmlT m x -> HtmlT m x
strong_ = el "strong"
{-# INLINE strong_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: HtmlBase m => HtmlT m x -> HtmlT m x
li_ = el "li"
{-# INLINE li_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h1_ = el "h1"
{-# INLINE h1_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h2_ = el "h2"
{-# INLINE h2_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h3_ = el "h3"
{-# INLINE h3_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h4_ = el "h4"
{-# INLINE h4_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h5_ = el "h5"
{-# INLINE h5_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h6_ = el "h6"
{-# INLINE h6_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
hr_ = el "hr"
{-# INLINE hr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: HtmlBase m => HtmlT m x -> HtmlT m x
pre_ = el "pre"
{-# INLINE pre_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: HtmlBase m => HtmlT m x -> HtmlT m x
input_ = el "input"
{-# INLINE input_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: HtmlBase m => HtmlT m x -> HtmlT m x
label_ = el "label"
{-# INLINE label_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: HtmlBase m => HtmlT m x -> HtmlT m x
a_ = el "a"
{-# INLINE a_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: HtmlBase m => HtmlT m x -> HtmlT m x
mark_ = el "mark"
{-# INLINE mark_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ruby_ = el "ruby"
{-# INLINE ruby_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: HtmlBase m => HtmlT m x -> HtmlT m x
rt_ = el "rt"
{-# INLINE rt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: HtmlBase m => HtmlT m x -> HtmlT m x
rp_ = el "rp"
{-# INLINE rp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: HtmlBase m => HtmlT m x -> HtmlT m x
bdi_ = el "bdi"
{-# INLINE bdi_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: HtmlBase m => HtmlT m x -> HtmlT m x
bdo_ = el "bdo"
{-# INLINE bdo_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
wbr_ = el "wbr"
{-# INLINE wbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: HtmlBase m => HtmlT m x -> HtmlT m x
details_ = el "details"
{-# INLINE details_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: HtmlBase m => HtmlT m x -> HtmlT m x
summary_ = el "summary"
{-# INLINE summary_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: HtmlBase m => HtmlT m x -> HtmlT m x
menuitem_ = el "menuitem"
{-# INLINE menuitem_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: HtmlBase m => HtmlT m x -> HtmlT m x
menu_ = el "menu"
{-# INLINE menu_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: HtmlBase m => HtmlT m x -> HtmlT m x
fieldset_ = el "fieldset"
{-# INLINE fieldset_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: HtmlBase m => HtmlT m x -> HtmlT m x
legend_ = el "legend"
{-# INLINE legend_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: HtmlBase m => HtmlT m x -> HtmlT m x
datalist_ = el "datalist"
{-# INLINE datalist_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: HtmlBase m => HtmlT m x -> HtmlT m x
optgroup_ = el "optgroup"
{-# INLINE optgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: HtmlBase m => HtmlT m x -> HtmlT m x
keygen_ = el "keygen"
{-# INLINE keygen_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: HtmlBase m => HtmlT m x -> HtmlT m x
output_ = el "output"
{-# INLINE output_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: HtmlBase m => HtmlT m x -> HtmlT m x
progress_ = el "progress"
{-# INLINE progress_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: HtmlBase m => HtmlT m x -> HtmlT m x
meter_ = el "meter"
{-# INLINE meter_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: HtmlBase m => HtmlT m x -> HtmlT m x
center_ = el "center"
{-# INLINE center_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: HtmlBase m => HtmlT m x -> HtmlT m x
audio_ = el "audio"
{-# INLINE audio_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: HtmlBase m => HtmlT m x -> HtmlT m x
video_ = el "video"
{-# INLINE video_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: HtmlBase m => HtmlT m x -> HtmlT m x
source_ = el "source"
{-# INLINE source_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: HtmlBase m => HtmlT m x -> HtmlT m x
track_ = el "track"
{-# INLINE track_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: HtmlBase m => HtmlT m x -> HtmlT m x
embed_ = el "embed"
{-# INLINE embed_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: HtmlBase m => HtmlT m x -> HtmlT m x
object_ = el "object"
{-# INLINE object_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: HtmlBase m => HtmlT m x -> HtmlT m x
param_ = el "param"
{-# INLINE param_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ins_ = el "ins"
{-# INLINE ins_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: HtmlBase m => HtmlT m x -> HtmlT m x
del_ = el "del"
{-# INLINE del_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: HtmlBase m => HtmlT m x -> HtmlT m x
small_ = el "small"
{-# INLINE small_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: HtmlBase m => HtmlT m x -> HtmlT m x
cite_ = el "cite"
{-# INLINE cite_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dfn_ = el "dfn"
{-# INLINE dfn_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
abbr_ = el "abbr"
{-# INLINE abbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: HtmlBase m => HtmlT m x -> HtmlT m x
time_ = el "time"
{-# INLINE time_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: HtmlBase m => HtmlT m x -> HtmlT m x
var_ = el "var"
{-# INLINE var_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: HtmlBase m => HtmlT m x -> HtmlT m x
samp_ = el "samp"
{-# INLINE samp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: HtmlBase m => HtmlT m x -> HtmlT m x
kbd_ = el "kbd"
{-# INLINE kbd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: HtmlBase m => HtmlT m x -> HtmlT m x
caption_ = el "caption"
{-# INLINE caption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: HtmlBase m => HtmlT m x -> HtmlT m x
colgroup_ = el "colgroup"
{-# INLINE colgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: HtmlBase m => HtmlT m x -> HtmlT m x
col_ = el "col"
{-# INLINE col_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: HtmlBase m => HtmlT m x -> HtmlT m x
nav_ = el "nav"
{-# INLINE nav_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: HtmlBase m => HtmlT m x -> HtmlT m x
article_ = el "article"
{-# INLINE article_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: HtmlBase m => HtmlT m x -> HtmlT m x
aside_ = el "aside"
{-# INLINE aside_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: HtmlBase m => HtmlT m x -> HtmlT m x
address_ = el "address"
{-# INLINE address_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: HtmlBase m => HtmlT m x -> HtmlT m x
main_ = el "main"
{-# INLINE main_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: HtmlBase m => HtmlT m x -> HtmlT m x
body_ = el "body"
{-# INLINE body_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: HtmlBase m => HtmlT m x -> HtmlT m x
figure_ = el "figure"
{-# INLINE figure_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: HtmlBase m => HtmlT m x -> HtmlT m x
figcaption_ = el "figcaption"
{-# INLINE figcaption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dl_ = el "dl"
{-# INLINE dl_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dt_ = el "dt"
{-# INLINE dt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dd_ = el "dd"
{-# INLINE dd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: HtmlBase m => HtmlT m x -> HtmlT m x
img_ = el "img"
{-# INLINE img_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: HtmlBase m => HtmlT m x -> HtmlT m x
iframe_ = el "iframe"
{-# INLINE iframe_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: HtmlBase m => HtmlT m x -> HtmlT m x
canvas_ = el "canvas"
{-# INLINE canvas_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: HtmlBase m => HtmlT m x -> HtmlT m x
math_ = el "math"
{-# INLINE math_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: HtmlBase m => HtmlT m x -> HtmlT m x
select_ = el "select"
{-# INLINE select_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: HtmlBase m => HtmlT m x -> HtmlT m x
option_ = el "option"
{-# INLINE option_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: HtmlBase m => HtmlT m x -> HtmlT m x
textarea_ = el "textarea"
{-# INLINE textarea_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: HtmlBase m => HtmlT m x -> HtmlT m x
sub_ = el "sub"
{-# INLINE sub_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: HtmlBase m => HtmlT m x -> HtmlT m x
sup_ = el "sup"
{-# INLINE sup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: HtmlBase m => HtmlT m x -> HtmlT m x
br_ = el "br"
{-# INLINE br_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ol_ = el "ol"
{-# INLINE ol_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: HtmlBase m => HtmlT m x -> HtmlT m x
blockquote_ = el "blockquote"
{-# INLINE blockquote_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: HtmlBase m => HtmlT m x -> HtmlT m x
code_ = el "code"
{-# INLINE code_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: HtmlBase m => HtmlT m x -> HtmlT m x
em_ = el "em"
{-# INLINE em_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: HtmlBase m => HtmlT m x -> HtmlT m x
i_ = el "i"
{-# INLINE i_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: HtmlBase m => HtmlT m x -> HtmlT m x
b_ = el "b"
{-# INLINE b_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: HtmlBase m => HtmlT m x -> HtmlT m x
u_ = el "u"
{-# INLINE u_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: HtmlBase m => HtmlT m x -> HtmlT m x
q_ = el "q"
{-# INLINE q_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: HtmlBase m => HtmlT m x -> HtmlT m x
script_ = el "script"
{-# INLINE script_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: HtmlBase m => HtmlT m x -> HtmlT m x
link_ = el "link"
{-# INLINE link_ #-}
