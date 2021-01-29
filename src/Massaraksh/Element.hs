{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Element@
-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
module Massaraksh.Element where

import Massaraksh.Types
import Massaraksh.Base
import Data.Text

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: Html x -> Html x
div_ = el "div"
{-# INLINE div_ #-}

divClass :: Text -> Html x -> Html x
divClass c = div_ . (("className" =: c) *>)
{-# INLINE divClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: Html x -> Html x
table_ = el "table"
{-# INLINE table_ #-}

tableClass :: Text -> Html x -> Html x
tableClass c = table_ . (("className" =: c) *>)
{-# INLINE tableClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: Html x -> Html x
thead_ = el "thead"
{-# INLINE thead_ #-}

theadClass :: Text -> Html x -> Html x
theadClass c = thead_ . (("className" =: c) *>)
{-# INLINE theadClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: Html x -> Html x
tbody_ = el "tbody"
{-# INLINE tbody_ #-}

tbodyClass :: Text -> Html x -> Html x
tbodyClass c = tbody_ . (("className" =: c) *>)
{-# INLINE tbodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: Html x -> Html x
tr_ = el "tr"
{-# INLINE tr_ #-}

trClass :: Text -> Html x -> Html x
trClass c = tr_ . (("className" =: c) *>)
{-# INLINE trClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: Html x -> Html x
th_ = el "th"
{-# INLINE th_ #-}

thClass :: Text -> Html x -> Html x
thClass c = th_ . (("className" =: c) *>)
{-# INLINE thClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: Html x -> Html x
td_ = el "td"
{-# INLINE td_ #-}

tdClass :: Text -> Html x -> Html x
tdClass c = td_ . (("className" =: c) *>)
{-# INLINE tdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: Html x -> Html x
tfoot_ = el "tfoot"
{-# INLINE tfoot_ #-}

tfootClass :: Text -> Html x -> Html x
tfootClass c = tfoot_ . (("className" =: c) *>)
{-# INLINE tfootClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: Html x -> Html x
section_ = el "section"
{-# INLINE section_ #-}

sectionClass :: Text -> Html x -> Html x
sectionClass c = section_ . (("className" =: c) *>)
{-# INLINE sectionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: Html x -> Html x
header_ = el "header"
{-# INLINE header_ #-}

headerClass :: Text -> Html x -> Html x
headerClass c = header_ . (("className" =: c) *>)
{-# INLINE headerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: Html x -> Html x
footer_ = el "footer"
{-# INLINE footer_ #-}

footerClass :: Text -> Html x -> Html x
footerClass c = footer_ . (("className" =: c) *>)
{-# INLINE footerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: Html x -> Html x
button_ = el "button"
{-# INLINE button_ #-}

buttonClass :: Text -> Html x -> Html x
buttonClass c = button_ . (("className" =: c) *>)
{-# INLINE buttonClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: Html x -> Html x
form_ = el "form"
{-# INLINE form_ #-}

formClass :: Text -> Html x -> Html x
formClass c = form_ . (("className" =: c) *>)
{-# INLINE formClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: Html x -> Html x
p_ = el "p"
{-# INLINE p_ #-}

pClass :: Text -> Html x -> Html x
pClass c = p_ . (("className" =: c) *>)
{-# INLINE pClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: Html x -> Html x
s_ = el "s"
{-# INLINE s_ #-}

sClass :: Text -> Html x -> Html x
sClass c = s_ . (("className" =: c) *>)
{-# INLINE sClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: Html x -> Html x
ul_ = el "ul"
{-# INLINE ul_ #-}

ulClass :: Text -> Html x -> Html x
ulClass c = ul_ . (("className" =: c) *>)
{-# INLINE ulClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: Html x -> Html x
span_ = el "span"
{-# INLINE span_ #-}

spanClass :: Text -> Html x -> Html x
spanClass c = span_ . (("className" =: c) *>)
{-# INLINE spanClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: Html x -> Html x
strong_ = el "strong"
{-# INLINE strong_ #-}

strongClass :: Text -> Html x -> Html x
strongClass c = strong_ . (("className" =: c) *>)
{-# INLINE strongClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: Html x -> Html x
li_ = el "li"
{-# INLINE li_ #-}

liClass :: Text -> Html x -> Html x
liClass c = li_ . (("className" =: c) *>)
{-# INLINE liClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: Html x -> Html x
h1_ = el "h1"
{-# INLINE h1_ #-}

h1Class :: Text -> Html x -> Html x
h1Class c = h1_ . (("className" =: c) *>)
{-# INLINE h1Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: Html x -> Html x
h2_ = el "h2"
{-# INLINE h2_ #-}

h2Class :: Text -> Html x -> Html x
h2Class c = h2_ . (("className" =: c) *>)
{-# INLINE h2Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: Html x -> Html x
h3_ = el "h3"
{-# INLINE h3_ #-}

h3Class :: Text -> Html x -> Html x
h3Class c = h3_ . (("className" =: c) *>)
{-# INLINE h3Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: Html x -> Html x
h4_ = el "h4"
{-# INLINE h4_ #-}

h4Class :: Text -> Html x -> Html x
h4Class c = h4_ . (("className" =: c) *>)
{-# INLINE h4Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: Html x -> Html x
h5_ = el "h5"
{-# INLINE h5_ #-}

h5Class :: Text -> Html x -> Html x
h5Class c = h5_ . (("className" =: c) *>)
{-# INLINE h5Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: Html x -> Html x
h6_ = el "h6"
{-# INLINE h6_ #-}

h6Class :: Text -> Html x -> Html x
h6Class c = h6_ . (("className" =: c) *>)
{-# INLINE h6Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: Html x -> Html x
hr_ = el "hr"
{-# INLINE hr_ #-}

hrClass :: Text -> Html x -> Html x
hrClass c = hr_ . (("className" =: c) *>)
{-# INLINE hrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: Html x -> Html x
pre_ = el "pre"
{-# INLINE pre_ #-}

preClass :: Text -> Html x -> Html x
preClass c = pre_ . (("className" =: c) *>)
{-# INLINE preClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: Html x -> Html x
input_ = el "input"
{-# INLINE input_ #-}

inputClass :: Text -> Html x -> Html x
inputClass c = input_ . (("className" =: c) *>)
{-# INLINE inputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: Html x -> Html x
label_ = el "label"
{-# INLINE label_ #-}

labelClass :: Text -> Html x -> Html x
labelClass c = label_ . (("className" =: c) *>)
{-# INLINE labelClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: Html x -> Html x
a_ = el "a"
{-# INLINE a_ #-}

aClass :: Text -> Html x -> Html x
aClass c = a_ . (("className" =: c) *>)
{-# INLINE aClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: Html x -> Html x
mark_ = el "mark"
{-# INLINE mark_ #-}

markClass :: Text -> Html x -> Html x
markClass c = mark_ . (("className" =: c) *>)
{-# INLINE markClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: Html x -> Html x
ruby_ = el "ruby"
{-# INLINE ruby_ #-}

rubyClass :: Text -> Html x -> Html x
rubyClass c = ruby_ . (("className" =: c) *>)
{-# INLINE rubyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: Html x -> Html x
rt_ = el "rt"
{-# INLINE rt_ #-}

rtClass :: Text -> Html x -> Html x
rtClass c = rt_ . (("className" =: c) *>)
{-# INLINE rtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: Html x -> Html x
rp_ = el "rp"
{-# INLINE rp_ #-}

rpClass :: Text -> Html x -> Html x
rpClass c = rp_ . (("className" =: c) *>)
{-# INLINE rpClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: Html x -> Html x
bdi_ = el "bdi"
{-# INLINE bdi_ #-}

bdiClass :: Text -> Html x -> Html x
bdiClass c = bdi_ . (("className" =: c) *>)
{-# INLINE bdiClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: Html x -> Html x
bdo_ = el "bdo"
{-# INLINE bdo_ #-}

bdoClass :: Text -> Html x -> Html x
bdoClass c = bdo_ . (("className" =: c) *>)
{-# INLINE bdoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: Html x -> Html x
wbr_ = el "wbr"
{-# INLINE wbr_ #-}

wbrClass :: Text -> Html x -> Html x
wbrClass c = wbr_ . (("className" =: c) *>)
{-# INLINE wbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: Html x -> Html x
details_ = el "details"
{-# INLINE details_ #-}

detailsClass :: Text -> Html x -> Html x
detailsClass c = details_ . (("className" =: c) *>)
{-# INLINE detailsClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: Html x -> Html x
summary_ = el "summary"
{-# INLINE summary_ #-}

summaryClass :: Text -> Html x -> Html x
summaryClass c = summary_ . (("className" =: c) *>)
{-# INLINE summaryClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: Html x -> Html x
menuitem_ = el "menuitem"
{-# INLINE menuitem_ #-}

menuitemClass :: Text -> Html x -> Html x
menuitemClass c = menuitem_ . (("className" =: c) *>)
{-# INLINE menuitemClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: Html x -> Html x
menu_ = el "menu"
{-# INLINE menu_ #-}

menuClass :: Text -> Html x -> Html x
menuClass c = menu_ . (("className" =: c) *>)
{-# INLINE menuClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: Html x -> Html x
fieldset_ = el "fieldset"
{-# INLINE fieldset_ #-}

fieldsetClass :: Text -> Html x -> Html x
fieldsetClass c = fieldset_ . (("className" =: c) *>)
{-# INLINE fieldsetClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: Html x -> Html x
legend_ = el "legend"
{-# INLINE legend_ #-}

legendClass :: Text -> Html x -> Html x
legendClass c = legend_ . (("className" =: c) *>)
{-# INLINE legendClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: Html x -> Html x
datalist_ = el "datalist"
{-# INLINE datalist_ #-}

datalistClass :: Text -> Html x -> Html x
datalistClass c = datalist_ . (("className" =: c) *>)
{-# INLINE datalistClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: Html x -> Html x
optgroup_ = el "optgroup"
{-# INLINE optgroup_ #-}

optgroupClass :: Text -> Html x -> Html x
optgroupClass c = optgroup_ . (("className" =: c) *>)
{-# INLINE optgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: Html x -> Html x
keygen_ = el "keygen"
{-# INLINE keygen_ #-}

keygenClass :: Text -> Html x -> Html x
keygenClass c = keygen_ . (("className" =: c) *>)
{-# INLINE keygenClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: Html x -> Html x
output_ = el "output"
{-# INLINE output_ #-}

outputClass :: Text -> Html x -> Html x
outputClass c = output_ . (("className" =: c) *>)
{-# INLINE outputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: Html x -> Html x
progress_ = el "progress"
{-# INLINE progress_ #-}

progressClass :: Text -> Html x -> Html x
progressClass c = progress_ . (("className" =: c) *>)
{-# INLINE progressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: Html x -> Html x
meter_ = el "meter"
{-# INLINE meter_ #-}

meterClass :: Text -> Html x -> Html x
meterClass c = meter_ . (("className" =: c) *>)
{-# INLINE meterClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: Html x -> Html x
center_ = el "center"
{-# INLINE center_ #-}

centerClass :: Text -> Html x -> Html x
centerClass c = center_ . (("className" =: c) *>)
{-# INLINE centerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: Html x -> Html x
audio_ = el "audio"
{-# INLINE audio_ #-}

audioClass :: Text -> Html x -> Html x
audioClass c = audio_ . (("className" =: c) *>)
{-# INLINE audioClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: Html x -> Html x
video_ = el "video"
{-# INLINE video_ #-}

videoClass :: Text -> Html x -> Html x
videoClass c = video_ . (("className" =: c) *>)
{-# INLINE videoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: Html x -> Html x
source_ = el "source"
{-# INLINE source_ #-}

sourceClass :: Text -> Html x -> Html x
sourceClass c = source_ . (("className" =: c) *>)
{-# INLINE sourceClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: Html x -> Html x
track_ = el "track"
{-# INLINE track_ #-}

trackClass :: Text -> Html x -> Html x
trackClass c = track_ . (("className" =: c) *>)
{-# INLINE trackClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: Html x -> Html x
embed_ = el "embed"
{-# INLINE embed_ #-}

embedClass :: Text -> Html x -> Html x
embedClass c = embed_ . (("className" =: c) *>)
{-# INLINE embedClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: Html x -> Html x
object_ = el "object"
{-# INLINE object_ #-}

objectClass :: Text -> Html x -> Html x
objectClass c = object_ . (("className" =: c) *>)
{-# INLINE objectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: Html x -> Html x
param_ = el "param"
{-# INLINE param_ #-}

paramClass :: Text -> Html x -> Html x
paramClass c = param_ . (("className" =: c) *>)
{-# INLINE paramClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: Html x -> Html x
ins_ = el "ins"
{-# INLINE ins_ #-}

insClass :: Text -> Html x -> Html x
insClass c = ins_ . (("className" =: c) *>)
{-# INLINE insClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: Html x -> Html x
del_ = el "del"
{-# INLINE del_ #-}

delClass :: Text -> Html x -> Html x
delClass c = del_ . (("className" =: c) *>)
{-# INLINE delClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: Html x -> Html x
small_ = el "small"
{-# INLINE small_ #-}

smallClass :: Text -> Html x -> Html x
smallClass c = small_ . (("className" =: c) *>)
{-# INLINE smallClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: Html x -> Html x
cite_ = el "cite"
{-# INLINE cite_ #-}

citeClass :: Text -> Html x -> Html x
citeClass c = cite_ . (("className" =: c) *>)
{-# INLINE citeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: Html x -> Html x
dfn_ = el "dfn"
{-# INLINE dfn_ #-}

dfnClass :: Text -> Html x -> Html x
dfnClass c = dfn_ . (("className" =: c) *>)
{-# INLINE dfnClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: Html x -> Html x
abbr_ = el "abbr"
{-# INLINE abbr_ #-}

abbrClass :: Text -> Html x -> Html x
abbrClass c = abbr_ . (("className" =: c) *>)
{-# INLINE abbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: Html x -> Html x
time_ = el "time"
{-# INLINE time_ #-}

timeClass :: Text -> Html x -> Html x
timeClass c = time_ . (("className" =: c) *>)
{-# INLINE timeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: Html x -> Html x
var_ = el "var"
{-# INLINE var_ #-}

varClass :: Text -> Html x -> Html x
varClass c = var_ . (("className" =: c) *>)
{-# INLINE varClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: Html x -> Html x
samp_ = el "samp"
{-# INLINE samp_ #-}

sampClass :: Text -> Html x -> Html x
sampClass c = samp_ . (("className" =: c) *>)
{-# INLINE sampClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: Html x -> Html x
kbd_ = el "kbd"
{-# INLINE kbd_ #-}

kbdClass :: Text -> Html x -> Html x
kbdClass c = kbd_ . (("className" =: c) *>)
{-# INLINE kbdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: Html x -> Html x
caption_ = el "caption"
{-# INLINE caption_ #-}

captionClass :: Text -> Html x -> Html x
captionClass c = caption_ . (("className" =: c) *>)
{-# INLINE captionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: Html x -> Html x
colgroup_ = el "colgroup"
{-# INLINE colgroup_ #-}

colgroupClass :: Text -> Html x -> Html x
colgroupClass c = colgroup_ . (("className" =: c) *>)
{-# INLINE colgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: Html x -> Html x
col_ = el "col"
{-# INLINE col_ #-}

colClass :: Text -> Html x -> Html x
colClass c = col_ . (("className" =: c) *>)
{-# INLINE colClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: Html x -> Html x
nav_ = el "nav"
{-# INLINE nav_ #-}

navClass :: Text -> Html x -> Html x
navClass c = nav_ . (("className" =: c) *>)
{-# INLINE navClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: Html x -> Html x
article_ = el "article"
{-# INLINE article_ #-}

articleClass :: Text -> Html x -> Html x
articleClass c = article_ . (("className" =: c) *>)
{-# INLINE articleClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: Html x -> Html x
aside_ = el "aside"
{-# INLINE aside_ #-}

asideClass :: Text -> Html x -> Html x
asideClass c = aside_ . (("className" =: c) *>)
{-# INLINE asideClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: Html x -> Html x
address_ = el "address"
{-# INLINE address_ #-}

addressClass :: Text -> Html x -> Html x
addressClass c = address_ . (("className" =: c) *>)
{-# INLINE addressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: Html x -> Html x
main_ = el "main"
{-# INLINE main_ #-}

mainClass :: Text -> Html x -> Html x
mainClass c = main_ . (("className" =: c) *>)
{-# INLINE mainClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: Html x -> Html x
body_ = el "body"
{-# INLINE body_ #-}

bodyClass :: Text -> Html x -> Html x
bodyClass c = body_ . (("className" =: c) *>)
{-# INLINE bodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: Html x -> Html x
figure_ = el "figure"
{-# INLINE figure_ #-}

figureClass :: Text -> Html x -> Html x
figureClass c = figure_ . (("className" =: c) *>)
{-# INLINE figureClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: Html x -> Html x
figcaption_ = el "figcaption"
{-# INLINE figcaption_ #-}

figcaptionClass :: Text -> Html x -> Html x
figcaptionClass c = figcaption_ . (("className" =: c) *>)
{-# INLINE figcaptionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: Html x -> Html x
dl_ = el "dl"
{-# INLINE dl_ #-}

dlClass :: Text -> Html x -> Html x
dlClass c = dl_ . (("className" =: c) *>)
{-# INLINE dlClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: Html x -> Html x
dt_ = el "dt"
{-# INLINE dt_ #-}

dtClass :: Text -> Html x -> Html x
dtClass c = dt_ . (("className" =: c) *>)
{-# INLINE dtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: Html x -> Html x
dd_ = el "dd"
{-# INLINE dd_ #-}

ddClass :: Text -> Html x -> Html x
ddClass c = dd_ . (("className" =: c) *>)
{-# INLINE ddClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: Html x -> Html x
img_ = el "img"
{-# INLINE img_ #-}

imgClass :: Text -> Html x -> Html x
imgClass c = img_ . (("className" =: c) *>)
{-# INLINE imgClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: Html x -> Html x
iframe_ = el "iframe"
{-# INLINE iframe_ #-}

iframeClass :: Text -> Html x -> Html x
iframeClass c = iframe_ . (("className" =: c) *>)
{-# INLINE iframeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: Html x -> Html x
canvas_ = el "canvas"
{-# INLINE canvas_ #-}

canvasClass :: Text -> Html x -> Html x
canvasClass c = canvas_ . (("className" =: c) *>)
{-# INLINE canvasClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: Html x -> Html x
math_ = el "math"
{-# INLINE math_ #-}

mathClass :: Text -> Html x -> Html x
mathClass c = math_ . (("className" =: c) *>)
{-# INLINE mathClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: Html x -> Html x
select_ = el "select"
{-# INLINE select_ #-}

selectClass :: Text -> Html x -> Html x
selectClass c = select_ . (("className" =: c) *>)
{-# INLINE selectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: Html x -> Html x
option_ = el "option"
{-# INLINE option_ #-}

optionClass :: Text -> Html x -> Html x
optionClass c = option_ . (("className" =: c) *>)
{-# INLINE optionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: Html x -> Html x
textarea_ = el "textarea"
{-# INLINE textarea_ #-}

textareaClass :: Text -> Html x -> Html x
textareaClass c = textarea_ . (("className" =: c) *>)
{-# INLINE textareaClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: Html x -> Html x
sub_ = el "sub"
{-# INLINE sub_ #-}

subClass :: Text -> Html x -> Html x
subClass c = sub_ . (("className" =: c) *>)
{-# INLINE subClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: Html x -> Html x
sup_ = el "sup"
{-# INLINE sup_ #-}

supClass :: Text -> Html x -> Html x
supClass c = sup_ . (("className" =: c) *>)
{-# INLINE supClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: Html x -> Html x
br_ = el "br"
{-# INLINE br_ #-}

brClass :: Text -> Html x -> Html x
brClass c = br_ . (("className" =: c) *>)
{-# INLINE brClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: Html x -> Html x
ol_ = el "ol"
{-# INLINE ol_ #-}

olClass :: Text -> Html x -> Html x
olClass c = ol_ . (("className" =: c) *>)
{-# INLINE olClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: Html x -> Html x
blockquote_ = el "blockquote"
{-# INLINE blockquote_ #-}

blockquoteClass :: Text -> Html x -> Html x
blockquoteClass c = blockquote_ . (("className" =: c) *>)
{-# INLINE blockquoteClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: Html x -> Html x
code_ = el "code"
{-# INLINE code_ #-}

codeClass :: Text -> Html x -> Html x
codeClass c = code_ . (("className" =: c) *>)
{-# INLINE codeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: Html x -> Html x
em_ = el "em"
{-# INLINE em_ #-}

emClass :: Text -> Html x -> Html x
emClass c = em_ . (("className" =: c) *>)
{-# INLINE emClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: Html x -> Html x
i_ = el "i"
{-# INLINE i_ #-}

iClass :: Text -> Html x -> Html x
iClass c = i_ . (("className" =: c) *>)
{-# INLINE iClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: Html x -> Html x
b_ = el "b"
{-# INLINE b_ #-}

bClass :: Text -> Html x -> Html x
bClass c = b_ . (("className" =: c) *>)
{-# INLINE bClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: Html x -> Html x
u_ = el "u"
{-# INLINE u_ #-}

uClass :: Text -> Html x -> Html x
uClass c = u_ . (("className" =: c) *>)
{-# INLINE uClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: Html x -> Html x
q_ = el "q"
{-# INLINE q_ #-}

qClass :: Text -> Html x -> Html x
qClass c = q_ . (("className" =: c) *>)
{-# INLINE qClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: Html x -> Html x
script_ = el "script"
{-# INLINE script_ #-}

scriptClass :: Text -> Html x -> Html x
scriptClass c = script_ . (("className" =: c) *>)
{-# INLINE scriptClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: Html x -> Html x
link_ = el "link"
{-# INLINE link_ #-}

linkClass :: Text -> Html x -> Html x
linkClass c = link_ . (("className" =: c) *>)
{-# INLINE linkClass #-}
