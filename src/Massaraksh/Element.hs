-- |Borrowed from @Miso.Html.Element@
-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
module Massaraksh.Element where

import Data.Text as T
import Massaraksh.Base
import Massaraksh.Types

class Term arg result | result -> arg where
  -- | Use this if you want to make an element which inserts some
  -- pre-prepared attributes into the element.
  term :: Text          -- ^ Name.
    -> arg           -- ^ Some argument.
    -> result        -- ^ Result: either an element or an attribute.

instance (f ~ HtmlT a) => Term [HtmlT ()] (f -> HtmlT a) where
  term name attrs = el name . (sequence_ attrs *>)
  {-# INLINE term #-}

-- | Given children immediately, just use that and expect no
-- attributes.
instance Term (HtmlT a) (HtmlT a) where
  term = el
  {-# INLINE term #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: Term arg result => arg -> result
div_ = term "div"
{-# INLINE div_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: Term arg result => arg -> result
table_ = term "table"
{-# INLINE table_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: Term arg result => arg -> result
thead_ = term "thead"
{-# INLINE thead_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: Term arg result => arg -> result
tbody_ = term "tbody"
{-# INLINE tbody_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: Term arg result => arg -> result
tr_ = term "tr"
{-# INLINE tr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: Term arg result => arg -> result
th_ = term "th"
{-# INLINE th_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: Term arg result => arg -> result
td_ = term "td"
{-# INLINE td_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: Term arg result => arg -> result
tfoot_ = term "tfoot"
{-# INLINE tfoot_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: Term arg result => arg -> result
section_ = term "section"
{-# INLINE section_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: Term arg result => arg -> result
header_ = term "header"
{-# INLINE header_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: Term arg result => arg -> result
footer_ = term "footer"
{-# INLINE footer_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: Term arg result => arg -> result
button_ = term "button"
{-# INLINE button_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: Term arg result => arg -> result
form_ = term "form"
{-# INLINE form_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: Term arg result => arg -> result
p_ = term "p"
{-# INLINE p_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: Term arg result => arg -> result
s_ = term "s"
{-# INLINE s_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: Term arg result => arg -> result
ul_ = term "ul"
{-# INLINE ul_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: Term arg result => arg -> result
span_ = term "span"
{-# INLINE span_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: Term arg result => arg -> result
strong_ = term "strong"
{-# INLINE strong_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: Term arg result => arg -> result
li_ = term "li"
{-# INLINE li_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: Term arg result => arg -> result
h1_ = term "h1"
{-# INLINE h1_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: Term arg result => arg -> result
h2_ = term "h2"
{-# INLINE h2_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: Term arg result => arg -> result
h3_ = term "h3"
{-# INLINE h3_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: Term arg result => arg -> result
h4_ = term "h4"
{-# INLINE h4_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: Term arg result => arg -> result
h5_ = term "h5"
{-# INLINE h5_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: Term arg result => arg -> result
h6_ = term "h6"
{-# INLINE h6_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: Term arg result => arg -> result
hr_ = term "hr"
{-# INLINE hr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: Term arg result => arg -> result
pre_ = term "pre"
{-# INLINE pre_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: Term arg result => arg -> result
input_ = term "input"
{-# INLINE input_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: Term arg result => arg -> result
label_ = term "label"
{-# INLINE label_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: Term arg result => arg -> result
a_ = term "a"
{-# INLINE a_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: Term arg result => arg -> result
mark_ = term "mark"
{-# INLINE mark_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: Term arg result => arg -> result
ruby_ = term "ruby"
{-# INLINE ruby_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: Term arg result => arg -> result
rt_ = term "rt"
{-# INLINE rt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: Term arg result => arg -> result
rp_ = term "rp"
{-# INLINE rp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: Term arg result => arg -> result
bdi_ = term "bdi"
{-# INLINE bdi_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: Term arg result => arg -> result
bdo_ = term "bdo"
{-# INLINE bdo_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: Term arg result => arg -> result
wbr_ = term "wbr"
{-# INLINE wbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: Term arg result => arg -> result
details_ = term "details"
{-# INLINE details_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: Term arg result => arg -> result
summary_ = term "summary"
{-# INLINE summary_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: Term arg result => arg -> result
menuitem_ = term "menuitem"
{-# INLINE menuitem_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: Term arg result => arg -> result
menu_ = term "menu"
{-# INLINE menu_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: Term arg result => arg -> result
fieldset_ = term "fieldset"
{-# INLINE fieldset_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: Term arg result => arg -> result
legend_ = term "legend"
{-# INLINE legend_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: Term arg result => arg -> result
datalist_ = term "datalist"
{-# INLINE datalist_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: Term arg result => arg -> result
optgroup_ = term "optgroup"
{-# INLINE optgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: Term arg result => arg -> result
keygen_ = term "keygen"
{-# INLINE keygen_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: Term arg result => arg -> result
output_ = term "output"
{-# INLINE output_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: Term arg result => arg -> result
progress_ = term "progress"
{-# INLINE progress_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: Term arg result => arg -> result
meter_ = term "meter"
{-# INLINE meter_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: Term arg result => arg -> result
center_ = term "center"
{-# INLINE center_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: Term arg result => arg -> result
audio_ = term "audio"
{-# INLINE audio_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: Term arg result => arg -> result
video_ = term "video"
{-# INLINE video_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: Term arg result => arg -> result
source_ = term "source"
{-# INLINE source_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: Term arg result => arg -> result
track_ = term "track"
{-# INLINE track_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: Term arg result => arg -> result
embed_ = term "embed"
{-# INLINE embed_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: Term arg result => arg -> result
object_ = term "object"
{-# INLINE object_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: Term arg result => arg -> result
param_ = term "param"
{-# INLINE param_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: Term arg result => arg -> result
ins_ = term "ins"
{-# INLINE ins_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: Term arg result => arg -> result
del_ = term "del"
{-# INLINE del_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: Term arg result => arg -> result
small_ = term "small"
{-# INLINE small_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: Term arg result => arg -> result
cite_ = term "cite"
{-# INLINE cite_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: Term arg result => arg -> result
dfn_ = term "dfn"
{-# INLINE dfn_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: Term arg result => arg -> result
abbr_ = term "abbr"
{-# INLINE abbr_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: Term arg result => arg -> result
time_ = term "time"
{-# INLINE time_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: Term arg result => arg -> result
var_ = term "var"
{-# INLINE var_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: Term arg result => arg -> result
samp_ = term "samp"
{-# INLINE samp_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: Term arg result => arg -> result
kbd_ = term "kbd"
{-# INLINE kbd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: Term arg result => arg -> result
caption_ = term "caption"
{-# INLINE caption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: Term arg result => arg -> result
colgroup_ = term "colgroup"
{-# INLINE colgroup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: Term arg result => arg -> result
col_ = term "col"
{-# INLINE col_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: Term arg result => arg -> result
nav_ = term "nav"
{-# INLINE nav_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: Term arg result => arg -> result
article_ = term "article"
{-# INLINE article_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: Term arg result => arg -> result
aside_ = term "aside"
{-# INLINE aside_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: Term arg result => arg -> result
address_ = term "address"
{-# INLINE address_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: Term arg result => arg -> result
main_ = term "main"
{-# INLINE main_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: Term arg result => arg -> result
body_ = term "body"
{-# INLINE body_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: Term arg result => arg -> result
figure_ = term "figure"
{-# INLINE figure_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: Term arg result => arg -> result
figcaption_ = term "figcaption"
{-# INLINE figcaption_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: Term arg result => arg -> result
dl_ = term "dl"
{-# INLINE dl_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: Term arg result => arg -> result
dt_ = term "dt"
{-# INLINE dt_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: Term arg result => arg -> result
dd_ = term "dd"
{-# INLINE dd_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: Term arg result => arg -> result
img_ = term "img"
{-# INLINE img_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: Term arg result => arg -> result
iframe_ = term "iframe"
{-# INLINE iframe_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: Term arg result => arg -> result
canvas_ = term "canvas"
{-# INLINE canvas_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: Term arg result => arg -> result
math_ = term "math"
{-# INLINE math_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: Term arg result => arg -> result
select_ = term "select"
{-# INLINE select_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: Term arg result => arg -> result
option_ = term "option"
{-# INLINE option_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: Term arg result => arg -> result
textarea_ = term "textarea"
{-# INLINE textarea_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: Term arg result => arg -> result
sub_ = term "sub"
{-# INLINE sub_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: Term arg result => arg -> result
sup_ = term "sup"
{-# INLINE sup_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: Term arg result => arg -> result
br_ = term "br"
{-# INLINE br_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: Term arg result => arg -> result
ol_ = term "ol"
{-# INLINE ol_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: Term arg result => arg -> result
blockquote_ = term "blockquote"
{-# INLINE blockquote_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: Term arg result => arg -> result
code_ = term "code"
{-# INLINE code_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: Term arg result => arg -> result
em_ = term "em"
{-# INLINE em_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: Term arg result => arg -> result
i_ = term "i"
{-# INLINE i_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: Term arg result => arg -> result
b_ = term "b"
{-# INLINE b_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: Term arg result => arg -> result
u_ = term "u"
{-# INLINE u_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: Term arg result => arg -> result
q_ = term "q"
{-# INLINE q_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: Term arg result => arg -> result
script_ = term "script"
{-# INLINE script_ #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: Term arg result => arg -> result
link_ = term "link"
{-# INLINE link_ #-}
