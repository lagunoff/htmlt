{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Element@
-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
module Massaraksh.Element where

import Massaraksh.Base

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
div_ = el "div"
{-# INLINE div_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
table_ = el "table"
{-# INLINE table_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
thead_ = el "thead"
{-# INLINE thead_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
tbody_ = el "tbody"
{-# INLINE tbody_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
tr_ = el "tr"
{-# INLINE tr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
th_ = el "th"
{-# INLINE th_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
td_ = el "td"
{-# INLINE td_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
tfoot_ = el "tfoot"
{-# INLINE tfoot_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
section_ = el "section"
{-# INLINE section_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
header_ = el "header"
{-# INLINE header_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
footer_ = el "footer"
{-# INLINE footer_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
button_ = el "button"
{-# INLINE button_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
form_ = el "form"
{-# INLINE form_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
p_ = el "p"
{-# INLINE p_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
s_ = el "s"
{-# INLINE s_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
ul_ = el "ul"
{-# INLINE ul_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
span_ = el "span"
{-# INLINE span_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
strong_ = el "strong"
{-# INLINE strong_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
li_ = el "li"
{-# INLINE li_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
h1_ = el "h1"
{-# INLINE h1_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
h2_ = el "h2"
{-# INLINE h2_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
h3_ = el "h3"
{-# INLINE h3_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
h4_ = el "h4"
{-# INLINE h4_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
h5_ = el "h5"
{-# INLINE h5_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
h6_ = el "h6"
{-# INLINE h6_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
hr_ = el "hr"
{-# INLINE hr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
pre_ = el "pre"
{-# INLINE pre_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
input_ = el "input"
{-# INLINE input_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
label_ = el "label"
{-# INLINE label_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
a_ = el "a"
{-# INLINE a_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
mark_ = el "mark"
{-# INLINE mark_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
ruby_ = el "ruby"
{-# INLINE ruby_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
rt_ = el "rt"
{-# INLINE rt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
rp_ = el "rp"
{-# INLINE rp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
bdi_ = el "bdi"
{-# INLINE bdi_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
bdo_ = el "bdo"
{-# INLINE bdo_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
wbr_ = el "wbr"
{-# INLINE wbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
details_ = el "details"
{-# INLINE details_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
summary_ = el "summary"
{-# INLINE summary_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
menuitem_ = el "menuitem"
{-# INLINE menuitem_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
menu_ = el "menu"
{-# INLINE menu_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
fieldset_ = el "fieldset"
{-# INLINE fieldset_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
legend_ = el "legend"
{-# INLINE legend_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
datalist_ = el "datalist"
{-# INLINE datalist_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
optgroup_ = el "optgroup"
{-# INLINE optgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
keygen_ = el "keygen"
{-# INLINE keygen_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
output_ = el "output"
{-# INLINE output_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
progress_ = el "progress"
{-# INLINE progress_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
meter_ = el "meter"
{-# INLINE meter_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
center_ = el "center"
{-# INLINE center_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
audio_ = el "audio"
{-# INLINE audio_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
video_ = el "video"
{-# INLINE video_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
source_ = el "source"
{-# INLINE source_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
track_ = el "track"
{-# INLINE track_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
embed_ = el "embed"
{-# INLINE embed_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
object_ = el "object"
{-# INLINE object_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
param_ = el "param"
{-# INLINE param_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
ins_ = el "ins"
{-# INLINE ins_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
del_ = el "del"
{-# INLINE del_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
small_ = el "small"
{-# INLINE small_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
cite_ = el "cite"
{-# INLINE cite_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
dfn_ = el "dfn"
{-# INLINE dfn_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
abbr_ = el "abbr"
{-# INLINE abbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
time_ = el "time"
{-# INLINE time_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
var_ = el "var"
{-# INLINE var_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
samp_ = el "samp"
{-# INLINE samp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
kbd_ = el "kbd"
{-# INLINE kbd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
caption_ = el "caption"
{-# INLINE caption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
colgroup_ = el "colgroup"
{-# INLINE colgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
col_ = el "col"
{-# INLINE col_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
nav_ = el "nav"
{-# INLINE nav_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
article_ = el "article"
{-# INLINE article_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
aside_ = el "aside"
{-# INLINE aside_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
address_ = el "address"
{-# INLINE address_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
main_ = el "main"
{-# INLINE main_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
body_ = el "body"
{-# INLINE body_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
figure_ = el "figure"
{-# INLINE figure_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
figcaption_ = el "figcaption"
{-# INLINE figcaption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
dl_ = el "dl"
{-# INLINE dl_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
dt_ = el "dt"
{-# INLINE dt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
dd_ = el "dd"
{-# INLINE dd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
img_ = el "img"
{-# INLINE img_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
iframe_ = el "iframe"
{-# INLINE iframe_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
canvas_ = el "canvas"
{-# INLINE canvas_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
math_ = el "math"
{-# INLINE math_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
select_ = el "select"
{-# INLINE select_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
option_ = el "option"
{-# INLINE option_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
textarea_ = el "textarea"
{-# INLINE textarea_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
sub_ = el "sub"
{-# INLINE sub_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
sup_ = el "sup"
{-# INLINE sup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
br_ = el "br"
{-# INLINE br_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
ol_ = el "ol"
{-# INLINE ol_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
blockquote_ = el "blockquote"
{-# INLINE blockquote_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
code_ = el "code"
{-# INLINE code_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
em_ = el "em"
{-# INLINE em_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
i_ = el "i"
{-# INLINE i_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
b_ = el "b"
{-# INLINE b_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
u_ = el "u"
{-# INLINE u_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
q_ = el "q"
{-# INLINE q_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
script_ = el "script"
{-# INLINE script_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: MonadHtmlBase m => HtmlT w s t m a -> HtmlT w s t m a
link_ = el "link"
{-# INLINE link_ #-}
