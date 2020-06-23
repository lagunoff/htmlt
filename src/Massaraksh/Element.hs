{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Borrowed from @Miso.Html.Element@
-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
module Massaraksh.Element where

import Massaraksh.Types
import Massaraksh.Base
import Data.JSString (JSString)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: HtmlBase m => HtmlT m x -> HtmlT m x
div_ = el "div"
{-# INLINE div_ #-}

divClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
divClass c = div_ . (("className" =: c) *>)
{-# INLINE divClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: HtmlBase m => HtmlT m x -> HtmlT m x
table_ = el "table"
{-# INLINE table_ #-}

tableClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
tableClass c = table_ . (("className" =: c) *>)
{-# INLINE tableClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: HtmlBase m => HtmlT m x -> HtmlT m x
thead_ = el "thead"
{-# INLINE thead_ #-}

theadClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
theadClass c = thead_ . (("className" =: c) *>)
{-# INLINE theadClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: HtmlBase m => HtmlT m x -> HtmlT m x
tbody_ = el "tbody"
{-# INLINE tbody_ #-}

tbodyClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
tbodyClass c = tbody_ . (("className" =: c) *>)
{-# INLINE tbodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
tr_ = el "tr"
{-# INLINE tr_ #-}

trClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
trClass c = tr_ . (("className" =: c) *>)
{-# INLINE trClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: HtmlBase m => HtmlT m x -> HtmlT m x
th_ = el "th"
{-# INLINE th_ #-}

thClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
thClass c = th_ . (("className" =: c) *>)
{-# INLINE thClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: HtmlBase m => HtmlT m x -> HtmlT m x
td_ = el "td"
{-# INLINE td_ #-}

tdClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
tdClass c = td_ . (("className" =: c) *>)
{-# INLINE tdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: HtmlBase m => HtmlT m x -> HtmlT m x
tfoot_ = el "tfoot"
{-# INLINE tfoot_ #-}

tfootClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
tfootClass c = tfoot_ . (("className" =: c) *>)
{-# INLINE tfootClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: HtmlBase m => HtmlT m x -> HtmlT m x
section_ = el "section"
{-# INLINE section_ #-}

sectionClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
sectionClass c = section_ . (("className" =: c) *>)
{-# INLINE sectionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: HtmlBase m => HtmlT m x -> HtmlT m x
header_ = el "header"
{-# INLINE header_ #-}

headerClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
headerClass c = header_ . (("className" =: c) *>)
{-# INLINE headerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: HtmlBase m => HtmlT m x -> HtmlT m x
footer_ = el "footer"
{-# INLINE footer_ #-}

footerClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
footerClass c = footer_ . (("className" =: c) *>)
{-# INLINE footerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: HtmlBase m => HtmlT m x -> HtmlT m x
button_ = el "button"
{-# INLINE button_ #-}

buttonClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
buttonClass c = button_ . (("className" =: c) *>)
{-# INLINE buttonClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: HtmlBase m => HtmlT m x -> HtmlT m x
form_ = el "form"
{-# INLINE form_ #-}

formClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
formClass c = form_ . (("className" =: c) *>)
{-# INLINE formClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: HtmlBase m => HtmlT m x -> HtmlT m x
p_ = el "p"
{-# INLINE p_ #-}

pClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
pClass c = p_ . (("className" =: c) *>)
{-# INLINE pClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: HtmlBase m => HtmlT m x -> HtmlT m x
s_ = el "s"
{-# INLINE s_ #-}

sClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
sClass c = s_ . (("className" =: c) *>)
{-# INLINE sClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ul_ = el "ul"
{-# INLINE ul_ #-}

ulClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
ulClass c = ul_ . (("className" =: c) *>)
{-# INLINE ulClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: HtmlBase m => HtmlT m x -> HtmlT m x
span_ = el "span"
{-# INLINE span_ #-}

spanClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
spanClass c = span_ . (("className" =: c) *>)
{-# INLINE spanClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: HtmlBase m => HtmlT m x -> HtmlT m x
strong_ = el "strong"
{-# INLINE strong_ #-}

strongClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
strongClass c = strong_ . (("className" =: c) *>)
{-# INLINE strongClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: HtmlBase m => HtmlT m x -> HtmlT m x
li_ = el "li"
{-# INLINE li_ #-}

liClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
liClass c = li_ . (("className" =: c) *>)
{-# INLINE liClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h1_ = el "h1"
{-# INLINE h1_ #-}

h1Class :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
h1Class c = h1_ . (("className" =: c) *>)
{-# INLINE h1Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h2_ = el "h2"
{-# INLINE h2_ #-}

h2Class :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
h2Class c = h2_ . (("className" =: c) *>)
{-# INLINE h2Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h3_ = el "h3"
{-# INLINE h3_ #-}

h3Class :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
h3Class c = h3_ . (("className" =: c) *>)
{-# INLINE h3Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h4_ = el "h4"
{-# INLINE h4_ #-}

h4Class :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
h4Class c = h4_ . (("className" =: c) *>)
{-# INLINE h4Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h5_ = el "h5"
{-# INLINE h5_ #-}

h5Class :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
h5Class c = h5_ . (("className" =: c) *>)
{-# INLINE h5Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: HtmlBase m => HtmlT m x -> HtmlT m x
h6_ = el "h6"
{-# INLINE h6_ #-}

h6Class :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
h6Class c = h6_ . (("className" =: c) *>)
{-# INLINE h6Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
hr_ = el "hr"
{-# INLINE hr_ #-}

hrClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
hrClass c = hr_ . (("className" =: c) *>)
{-# INLINE hrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: HtmlBase m => HtmlT m x -> HtmlT m x
pre_ = el "pre"
{-# INLINE pre_ #-}

preClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
preClass c = pre_ . (("className" =: c) *>)
{-# INLINE preClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: HtmlBase m => HtmlT m x -> HtmlT m x
input_ = el "input"
{-# INLINE input_ #-}

inputClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
inputClass c = input_ . (("className" =: c) *>)
{-# INLINE inputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: HtmlBase m => HtmlT m x -> HtmlT m x
label_ = el "label"
{-# INLINE label_ #-}

labelClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
labelClass c = label_ . (("className" =: c) *>)
{-# INLINE labelClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: HtmlBase m => HtmlT m x -> HtmlT m x
a_ = el "a"
{-# INLINE a_ #-}

aClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
aClass c = a_ . (("className" =: c) *>)
{-# INLINE aClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: HtmlBase m => HtmlT m x -> HtmlT m x
mark_ = el "mark"
{-# INLINE mark_ #-}

markClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
markClass c = mark_ . (("className" =: c) *>)
{-# INLINE markClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ruby_ = el "ruby"
{-# INLINE ruby_ #-}

rubyClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
rubyClass c = ruby_ . (("className" =: c) *>)
{-# INLINE rubyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: HtmlBase m => HtmlT m x -> HtmlT m x
rt_ = el "rt"
{-# INLINE rt_ #-}

rtClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
rtClass c = rt_ . (("className" =: c) *>)
{-# INLINE rtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: HtmlBase m => HtmlT m x -> HtmlT m x
rp_ = el "rp"
{-# INLINE rp_ #-}

rpClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
rpClass c = rp_ . (("className" =: c) *>)
{-# INLINE rpClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: HtmlBase m => HtmlT m x -> HtmlT m x
bdi_ = el "bdi"
{-# INLINE bdi_ #-}

bdiClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
bdiClass c = bdi_ . (("className" =: c) *>)
{-# INLINE bdiClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: HtmlBase m => HtmlT m x -> HtmlT m x
bdo_ = el "bdo"
{-# INLINE bdo_ #-}

bdoClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
bdoClass c = bdo_ . (("className" =: c) *>)
{-# INLINE bdoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
wbr_ = el "wbr"
{-# INLINE wbr_ #-}

wbrClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
wbrClass c = wbr_ . (("className" =: c) *>)
{-# INLINE wbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: HtmlBase m => HtmlT m x -> HtmlT m x
details_ = el "details"
{-# INLINE details_ #-}

detailsClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
detailsClass c = details_ . (("className" =: c) *>)
{-# INLINE detailsClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: HtmlBase m => HtmlT m x -> HtmlT m x
summary_ = el "summary"
{-# INLINE summary_ #-}

summaryClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
summaryClass c = summary_ . (("className" =: c) *>)
{-# INLINE summaryClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: HtmlBase m => HtmlT m x -> HtmlT m x
menuitem_ = el "menuitem"
{-# INLINE menuitem_ #-}

menuitemClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
menuitemClass c = menuitem_ . (("className" =: c) *>)
{-# INLINE menuitemClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: HtmlBase m => HtmlT m x -> HtmlT m x
menu_ = el "menu"
{-# INLINE menu_ #-}

menuClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
menuClass c = menu_ . (("className" =: c) *>)
{-# INLINE menuClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: HtmlBase m => HtmlT m x -> HtmlT m x
fieldset_ = el "fieldset"
{-# INLINE fieldset_ #-}

fieldsetClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
fieldsetClass c = fieldset_ . (("className" =: c) *>)
{-# INLINE fieldsetClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: HtmlBase m => HtmlT m x -> HtmlT m x
legend_ = el "legend"
{-# INLINE legend_ #-}

legendClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
legendClass c = legend_ . (("className" =: c) *>)
{-# INLINE legendClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: HtmlBase m => HtmlT m x -> HtmlT m x
datalist_ = el "datalist"
{-# INLINE datalist_ #-}

datalistClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
datalistClass c = datalist_ . (("className" =: c) *>)
{-# INLINE datalistClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: HtmlBase m => HtmlT m x -> HtmlT m x
optgroup_ = el "optgroup"
{-# INLINE optgroup_ #-}

optgroupClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
optgroupClass c = optgroup_ . (("className" =: c) *>)
{-# INLINE optgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: HtmlBase m => HtmlT m x -> HtmlT m x
keygen_ = el "keygen"
{-# INLINE keygen_ #-}

keygenClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
keygenClass c = keygen_ . (("className" =: c) *>)
{-# INLINE keygenClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: HtmlBase m => HtmlT m x -> HtmlT m x
output_ = el "output"
{-# INLINE output_ #-}

outputClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
outputClass c = output_ . (("className" =: c) *>)
{-# INLINE outputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: HtmlBase m => HtmlT m x -> HtmlT m x
progress_ = el "progress"
{-# INLINE progress_ #-}

progressClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
progressClass c = progress_ . (("className" =: c) *>)
{-# INLINE progressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: HtmlBase m => HtmlT m x -> HtmlT m x
meter_ = el "meter"
{-# INLINE meter_ #-}

meterClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
meterClass c = meter_ . (("className" =: c) *>)
{-# INLINE meterClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: HtmlBase m => HtmlT m x -> HtmlT m x
center_ = el "center"
{-# INLINE center_ #-}

centerClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
centerClass c = center_ . (("className" =: c) *>)
{-# INLINE centerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: HtmlBase m => HtmlT m x -> HtmlT m x
audio_ = el "audio"
{-# INLINE audio_ #-}

audioClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
audioClass c = audio_ . (("className" =: c) *>)
{-# INLINE audioClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: HtmlBase m => HtmlT m x -> HtmlT m x
video_ = el "video"
{-# INLINE video_ #-}

videoClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
videoClass c = video_ . (("className" =: c) *>)
{-# INLINE videoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: HtmlBase m => HtmlT m x -> HtmlT m x
source_ = el "source"
{-# INLINE source_ #-}

sourceClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
sourceClass c = source_ . (("className" =: c) *>)
{-# INLINE sourceClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: HtmlBase m => HtmlT m x -> HtmlT m x
track_ = el "track"
{-# INLINE track_ #-}

trackClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
trackClass c = track_ . (("className" =: c) *>)
{-# INLINE trackClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: HtmlBase m => HtmlT m x -> HtmlT m x
embed_ = el "embed"
{-# INLINE embed_ #-}

embedClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
embedClass c = embed_ . (("className" =: c) *>)
{-# INLINE embedClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: HtmlBase m => HtmlT m x -> HtmlT m x
object_ = el "object"
{-# INLINE object_ #-}

objectClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
objectClass c = object_ . (("className" =: c) *>)
{-# INLINE objectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: HtmlBase m => HtmlT m x -> HtmlT m x
param_ = el "param"
{-# INLINE param_ #-}

paramClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
paramClass c = param_ . (("className" =: c) *>)
{-# INLINE paramClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ins_ = el "ins"
{-# INLINE ins_ #-}

insClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
insClass c = ins_ . (("className" =: c) *>)
{-# INLINE insClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: HtmlBase m => HtmlT m x -> HtmlT m x
del_ = el "del"
{-# INLINE del_ #-}

delClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
delClass c = del_ . (("className" =: c) *>)
{-# INLINE delClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: HtmlBase m => HtmlT m x -> HtmlT m x
small_ = el "small"
{-# INLINE small_ #-}

smallClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
smallClass c = small_ . (("className" =: c) *>)
{-# INLINE smallClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: HtmlBase m => HtmlT m x -> HtmlT m x
cite_ = el "cite"
{-# INLINE cite_ #-}

citeClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
citeClass c = cite_ . (("className" =: c) *>)
{-# INLINE citeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dfn_ = el "dfn"
{-# INLINE dfn_ #-}

dfnClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
dfnClass c = dfn_ . (("className" =: c) *>)
{-# INLINE dfnClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: HtmlBase m => HtmlT m x -> HtmlT m x
abbr_ = el "abbr"
{-# INLINE abbr_ #-}

abbrClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
abbrClass c = abbr_ . (("className" =: c) *>)
{-# INLINE abbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: HtmlBase m => HtmlT m x -> HtmlT m x
time_ = el "time"
{-# INLINE time_ #-}

timeClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
timeClass c = time_ . (("className" =: c) *>)
{-# INLINE timeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: HtmlBase m => HtmlT m x -> HtmlT m x
var_ = el "var"
{-# INLINE var_ #-}

varClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
varClass c = var_ . (("className" =: c) *>)
{-# INLINE varClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: HtmlBase m => HtmlT m x -> HtmlT m x
samp_ = el "samp"
{-# INLINE samp_ #-}

sampClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
sampClass c = samp_ . (("className" =: c) *>)
{-# INLINE sampClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: HtmlBase m => HtmlT m x -> HtmlT m x
kbd_ = el "kbd"
{-# INLINE kbd_ #-}

kbdClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
kbdClass c = kbd_ . (("className" =: c) *>)
{-# INLINE kbdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: HtmlBase m => HtmlT m x -> HtmlT m x
caption_ = el "caption"
{-# INLINE caption_ #-}

captionClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
captionClass c = caption_ . (("className" =: c) *>)
{-# INLINE captionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: HtmlBase m => HtmlT m x -> HtmlT m x
colgroup_ = el "colgroup"
{-# INLINE colgroup_ #-}

colgroupClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
colgroupClass c = colgroup_ . (("className" =: c) *>)
{-# INLINE colgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: HtmlBase m => HtmlT m x -> HtmlT m x
col_ = el "col"
{-# INLINE col_ #-}

colClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
colClass c = col_ . (("className" =: c) *>)
{-# INLINE colClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: HtmlBase m => HtmlT m x -> HtmlT m x
nav_ = el "nav"
{-# INLINE nav_ #-}

navClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
navClass c = nav_ . (("className" =: c) *>)
{-# INLINE navClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: HtmlBase m => HtmlT m x -> HtmlT m x
article_ = el "article"
{-# INLINE article_ #-}

articleClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
articleClass c = article_ . (("className" =: c) *>)
{-# INLINE articleClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: HtmlBase m => HtmlT m x -> HtmlT m x
aside_ = el "aside"
{-# INLINE aside_ #-}

asideClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
asideClass c = aside_ . (("className" =: c) *>)
{-# INLINE asideClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: HtmlBase m => HtmlT m x -> HtmlT m x
address_ = el "address"
{-# INLINE address_ #-}

addressClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
addressClass c = address_ . (("className" =: c) *>)
{-# INLINE addressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: HtmlBase m => HtmlT m x -> HtmlT m x
main_ = el "main"
{-# INLINE main_ #-}

mainClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
mainClass c = main_ . (("className" =: c) *>)
{-# INLINE mainClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: HtmlBase m => HtmlT m x -> HtmlT m x
body_ = el "body"
{-# INLINE body_ #-}

bodyClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
bodyClass c = body_ . (("className" =: c) *>)
{-# INLINE bodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: HtmlBase m => HtmlT m x -> HtmlT m x
figure_ = el "figure"
{-# INLINE figure_ #-}

figureClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
figureClass c = figure_ . (("className" =: c) *>)
{-# INLINE figureClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: HtmlBase m => HtmlT m x -> HtmlT m x
figcaption_ = el "figcaption"
{-# INLINE figcaption_ #-}

figcaptionClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
figcaptionClass c = figcaption_ . (("className" =: c) *>)
{-# INLINE figcaptionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dl_ = el "dl"
{-# INLINE dl_ #-}

dlClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
dlClass c = dl_ . (("className" =: c) *>)
{-# INLINE dlClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dt_ = el "dt"
{-# INLINE dt_ #-}

dtClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
dtClass c = dt_ . (("className" =: c) *>)
{-# INLINE dtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: HtmlBase m => HtmlT m x -> HtmlT m x
dd_ = el "dd"
{-# INLINE dd_ #-}

ddClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
ddClass c = dd_ . (("className" =: c) *>)
{-# INLINE ddClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: HtmlBase m => HtmlT m x -> HtmlT m x
img_ = el "img"
{-# INLINE img_ #-}

imgClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
imgClass c = img_ . (("className" =: c) *>)
{-# INLINE imgClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: HtmlBase m => HtmlT m x -> HtmlT m x
iframe_ = el "iframe"
{-# INLINE iframe_ #-}

iframeClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
iframeClass c = iframe_ . (("className" =: c) *>)
{-# INLINE iframeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: HtmlBase m => HtmlT m x -> HtmlT m x
canvas_ = el "canvas"
{-# INLINE canvas_ #-}

canvasClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
canvasClass c = canvas_ . (("className" =: c) *>)
{-# INLINE canvasClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: HtmlBase m => HtmlT m x -> HtmlT m x
math_ = el "math"
{-# INLINE math_ #-}

mathClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
mathClass c = math_ . (("className" =: c) *>)
{-# INLINE mathClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: HtmlBase m => HtmlT m x -> HtmlT m x
select_ = el "select"
{-# INLINE select_ #-}

selectClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
selectClass c = select_ . (("className" =: c) *>)
{-# INLINE selectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: HtmlBase m => HtmlT m x -> HtmlT m x
option_ = el "option"
{-# INLINE option_ #-}

optionClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
optionClass c = option_ . (("className" =: c) *>)
{-# INLINE optionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: HtmlBase m => HtmlT m x -> HtmlT m x
textarea_ = el "textarea"
{-# INLINE textarea_ #-}

textareaClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
textareaClass c = textarea_ . (("className" =: c) *>)
{-# INLINE textareaClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: HtmlBase m => HtmlT m x -> HtmlT m x
sub_ = el "sub"
{-# INLINE sub_ #-}

subClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
subClass c = sub_ . (("className" =: c) *>)
{-# INLINE subClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: HtmlBase m => HtmlT m x -> HtmlT m x
sup_ = el "sup"
{-# INLINE sup_ #-}

supClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
supClass c = sup_ . (("className" =: c) *>)
{-# INLINE supClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: HtmlBase m => HtmlT m x -> HtmlT m x
br_ = el "br"
{-# INLINE br_ #-}

brClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
brClass c = br_ . (("className" =: c) *>)
{-# INLINE brClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: HtmlBase m => HtmlT m x -> HtmlT m x
ol_ = el "ol"
{-# INLINE ol_ #-}

olClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
olClass c = ol_ . (("className" =: c) *>)
{-# INLINE olClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: HtmlBase m => HtmlT m x -> HtmlT m x
blockquote_ = el "blockquote"
{-# INLINE blockquote_ #-}

blockquoteClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
blockquoteClass c = blockquote_ . (("className" =: c) *>)
{-# INLINE blockquoteClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: HtmlBase m => HtmlT m x -> HtmlT m x
code_ = el "code"
{-# INLINE code_ #-}

codeClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
codeClass c = code_ . (("className" =: c) *>)
{-# INLINE codeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: HtmlBase m => HtmlT m x -> HtmlT m x
em_ = el "em"
{-# INLINE em_ #-}

emClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
emClass c = em_ . (("className" =: c) *>)
{-# INLINE emClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: HtmlBase m => HtmlT m x -> HtmlT m x
i_ = el "i"
{-# INLINE i_ #-}

iClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
iClass c = i_ . (("className" =: c) *>)
{-# INLINE iClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: HtmlBase m => HtmlT m x -> HtmlT m x
b_ = el "b"
{-# INLINE b_ #-}

bClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
bClass c = b_ . (("className" =: c) *>)
{-# INLINE bClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: HtmlBase m => HtmlT m x -> HtmlT m x
u_ = el "u"
{-# INLINE u_ #-}

uClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
uClass c = u_ . (("className" =: c) *>)
{-# INLINE uClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: HtmlBase m => HtmlT m x -> HtmlT m x
q_ = el "q"
{-# INLINE q_ #-}

qClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
qClass c = q_ . (("className" =: c) *>)
{-# INLINE qClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: HtmlBase m => HtmlT m x -> HtmlT m x
script_ = el "script"
{-# INLINE script_ #-}

scriptClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
scriptClass c = script_ . (("className" =: c) *>)
{-# INLINE scriptClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: HtmlBase m => HtmlT m x -> HtmlT m x
link_ = el "link"
{-# INLINE link_ #-}

linkClass :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
linkClass c = link_ . (("className" =: c) *>)
{-# INLINE linkClass #-}
