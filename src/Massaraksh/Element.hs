{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Element@
-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
module Massaraksh.Element where

import Massaraksh.Base

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: MonadWidget e m => m a -> m a
div_ = el "div"
{-# INLINE div_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: MonadWidget e m => m a -> m a
table_ = el "table"
{-# INLINE table_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: MonadWidget e m => m a -> m a
thead_ = el "thead"
{-# INLINE thead_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: MonadWidget e m => m a -> m a
tbody_ = el "tbody"
{-# INLINE tbody_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: MonadWidget e m => m a -> m a
tr_ = el "tr"
{-# INLINE tr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: MonadWidget e m => m a -> m a
th_ = el "th"
{-# INLINE th_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: MonadWidget e m => m a -> m a
td_ = el "td"
{-# INLINE td_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: MonadWidget e m => m a -> m a
tfoot_ = el "tfoot"
{-# INLINE tfoot_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: MonadWidget e m => m a -> m a
section_ = el "section"
{-# INLINE section_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: MonadWidget e m => m a -> m a
header_ = el "header"
{-# INLINE header_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: MonadWidget e m => m a -> m a
footer_ = el "footer"
{-# INLINE footer_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: MonadWidget e m => m a -> m a
button_ = el "button"
{-# INLINE button_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: MonadWidget e m => m a -> m a
form_ = el "form"
{-# INLINE form_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: MonadWidget e m => m a -> m a
p_ = el "p"
{-# INLINE p_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: MonadWidget e m => m a -> m a
s_ = el "s"
{-# INLINE s_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: MonadWidget e m => m a -> m a
ul_ = el "ul"
{-# INLINE ul_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: MonadWidget e m => m a -> m a
span_ = el "span"
{-# INLINE span_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: MonadWidget e m => m a -> m a
strong_ = el "strong"
{-# INLINE strong_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: MonadWidget e m => m a -> m a
li_ = el "li"
{-# INLINE li_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: MonadWidget e m => m a -> m a
h1_ = el "h1"
{-# INLINE h1_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: MonadWidget e m => m a -> m a
h2_ = el "h2"
{-# INLINE h2_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: MonadWidget e m => m a -> m a
h3_ = el "h3"
{-# INLINE h3_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: MonadWidget e m => m a -> m a
h4_ = el "h4"
{-# INLINE h4_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: MonadWidget e m => m a -> m a
h5_ = el "h5"
{-# INLINE h5_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: MonadWidget e m => m a -> m a
h6_ = el "h6"
{-# INLINE h6_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: MonadWidget e m => m a -> m a
hr_ = el "hr"
{-# INLINE hr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: MonadWidget e m => m a -> m a
pre_ = el "pre"
{-# INLINE pre_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: MonadWidget e m => m a -> m a
input_ = el "input"
{-# INLINE input_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: MonadWidget e m => m a -> m a
label_ = el "label"
{-# INLINE label_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: MonadWidget e m => m a -> m a
a_ = el "a"
{-# INLINE a_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: MonadWidget e m => m a -> m a
mark_ = el "mark"
{-# INLINE mark_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: MonadWidget e m => m a -> m a
ruby_ = el "ruby"
{-# INLINE ruby_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: MonadWidget e m => m a -> m a
rt_ = el "rt"
{-# INLINE rt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: MonadWidget e m => m a -> m a
rp_ = el "rp"
{-# INLINE rp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: MonadWidget e m => m a -> m a
bdi_ = el "bdi"
{-# INLINE bdi_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: MonadWidget e m => m a -> m a
bdo_ = el "bdo"
{-# INLINE bdo_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: MonadWidget e m => m a -> m a
wbr_ = el "wbr"
{-# INLINE wbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: MonadWidget e m => m a -> m a
details_ = el "details"
{-# INLINE details_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: MonadWidget e m => m a -> m a
summary_ = el "summary"
{-# INLINE summary_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: MonadWidget e m => m a -> m a
menuitem_ = el "menuitem"
{-# INLINE menuitem_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: MonadWidget e m => m a -> m a
menu_ = el "menu"
{-# INLINE menu_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: MonadWidget e m => m a -> m a
fieldset_ = el "fieldset"
{-# INLINE fieldset_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: MonadWidget e m => m a -> m a
legend_ = el "legend"
{-# INLINE legend_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: MonadWidget e m => m a -> m a
datalist_ = el "datalist"
{-# INLINE datalist_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: MonadWidget e m => m a -> m a
optgroup_ = el "optgroup"
{-# INLINE optgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: MonadWidget e m => m a -> m a
keygen_ = el "keygen"
{-# INLINE keygen_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: MonadWidget e m => m a -> m a
output_ = el "output"
{-# INLINE output_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: MonadWidget e m => m a -> m a
progress_ = el "progress"
{-# INLINE progress_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: MonadWidget e m => m a -> m a
meter_ = el "meter"
{-# INLINE meter_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: MonadWidget e m => m a -> m a
center_ = el "center"
{-# INLINE center_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: MonadWidget e m => m a -> m a
audio_ = el "audio"
{-# INLINE audio_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: MonadWidget e m => m a -> m a
video_ = el "video"
{-# INLINE video_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: MonadWidget e m => m a -> m a
source_ = el "source"
{-# INLINE source_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: MonadWidget e m => m a -> m a
track_ = el "track"
{-# INLINE track_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: MonadWidget e m => m a -> m a
embed_ = el "embed"
{-# INLINE embed_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: MonadWidget e m => m a -> m a
object_ = el "object"
{-# INLINE object_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: MonadWidget e m => m a -> m a
param_ = el "param"
{-# INLINE param_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: MonadWidget e m => m a -> m a
ins_ = el "ins"
{-# INLINE ins_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: MonadWidget e m => m a -> m a
del_ = el "del"
{-# INLINE del_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: MonadWidget e m => m a -> m a
small_ = el "small"
{-# INLINE small_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: MonadWidget e m => m a -> m a
cite_ = el "cite"
{-# INLINE cite_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: MonadWidget e m => m a -> m a
dfn_ = el "dfn"
{-# INLINE dfn_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: MonadWidget e m => m a -> m a
abbr_ = el "abbr"
{-# INLINE abbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: MonadWidget e m => m a -> m a
time_ = el "time"
{-# INLINE time_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: MonadWidget e m => m a -> m a
var_ = el "var"
{-# INLINE var_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: MonadWidget e m => m a -> m a
samp_ = el "samp"
{-# INLINE samp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: MonadWidget e m => m a -> m a
kbd_ = el "kbd"
{-# INLINE kbd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: MonadWidget e m => m a -> m a
caption_ = el "caption"
{-# INLINE caption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: MonadWidget e m => m a -> m a
colgroup_ = el "colgroup"
{-# INLINE colgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: MonadWidget e m => m a -> m a
col_ = el "col"
{-# INLINE col_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: MonadWidget e m => m a -> m a
nav_ = el "nav"
{-# INLINE nav_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: MonadWidget e m => m a -> m a
article_ = el "article"
{-# INLINE article_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: MonadWidget e m => m a -> m a
aside_ = el "aside"
{-# INLINE aside_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: MonadWidget e m => m a -> m a
address_ = el "address"
{-# INLINE address_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: MonadWidget e m => m a -> m a
main_ = el "main"
{-# INLINE main_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: MonadWidget e m => m a -> m a
body_ = el "body"
{-# INLINE body_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: MonadWidget e m => m a -> m a
figure_ = el "figure"
{-# INLINE figure_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: MonadWidget e m => m a -> m a
figcaption_ = el "figcaption"
{-# INLINE figcaption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: MonadWidget e m => m a -> m a
dl_ = el "dl"
{-# INLINE dl_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: MonadWidget e m => m a -> m a
dt_ = el "dt"
{-# INLINE dt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: MonadWidget e m => m a -> m a
dd_ = el "dd"
{-# INLINE dd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: MonadWidget e m => m a -> m a
img_ = el "img"
{-# INLINE img_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: MonadWidget e m => m a -> m a
iframe_ = el "iframe"
{-# INLINE iframe_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: MonadWidget e m => m a -> m a
canvas_ = el "canvas"
{-# INLINE canvas_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: MonadWidget e m => m a -> m a
math_ = el "math"
{-# INLINE math_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: MonadWidget e m => m a -> m a
select_ = el "select"
{-# INLINE select_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: MonadWidget e m => m a -> m a
option_ = el "option"
{-# INLINE option_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: MonadWidget e m => m a -> m a
textarea_ = el "textarea"
{-# INLINE textarea_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: MonadWidget e m => m a -> m a
sub_ = el "sub"
{-# INLINE sub_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: MonadWidget e m => m a -> m a
sup_ = el "sup"
{-# INLINE sup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: MonadWidget e m => m a -> m a
br_ = el "br"
{-# INLINE br_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: MonadWidget e m => m a -> m a
ol_ = el "ol"
{-# INLINE ol_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: MonadWidget e m => m a -> m a
blockquote_ = el "blockquote"
{-# INLINE blockquote_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: MonadWidget e m => m a -> m a
code_ = el "code"
{-# INLINE code_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: MonadWidget e m => m a -> m a
em_ = el "em"
{-# INLINE em_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: MonadWidget e m => m a -> m a
i_ = el "i"
{-# INLINE i_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: MonadWidget e m => m a -> m a
b_ = el "b"
{-# INLINE b_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: MonadWidget e m => m a -> m a
u_ = el "u"
{-# INLINE u_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: MonadWidget e m => m a -> m a
q_ = el "q"
{-# INLINE q_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: MonadWidget e m => m a -> m a
script_ = el "script"
{-# INLINE script_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: MonadWidget e m => m a -> m a
link_ = el "link"
{-# INLINE link_ #-}
