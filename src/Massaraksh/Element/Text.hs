module Massaraksh.Element.Text where

import Data.JSString.Text as JSS
import Data.Text
import Massaraksh.Types
import qualified Massaraksh.Element as H

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
divClass :: Text -> Html x -> Html x
divClass = H.divClass . JSS.textToJSString
{-# INLINE divClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
tableClass :: Text -> Html x -> Html x
tableClass = H.tableClass . JSS.textToJSString
{-# INLINE tableClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
theadClass :: Text -> Html x -> Html x
theadClass = H.theadClass . JSS.textToJSString
{-# INLINE theadClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbodyClass :: Text -> Html x -> Html x
tbodyClass = H.tbodyClass . JSS.textToJSString
{-# INLINE tbodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
trClass :: Text -> Html x -> Html x
trClass = H.trClass . JSS.textToJSString
{-# INLINE trClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
thClass :: Text -> Html x -> Html x
thClass = H.thClass . JSS.textToJSString
{-# INLINE thClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
tdClass :: Text -> Html x -> Html x
tdClass = H.tdClass . JSS.textToJSString
{-# INLINE tdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfootClass :: Text -> Html x -> Html x
tfootClass = H.tfootClass . JSS.textToJSString
{-# INLINE tfootClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
sectionClass :: Text -> Html x -> Html x
sectionClass = H.sectionClass . JSS.textToJSString
{-# INLINE sectionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
headerClass :: Text -> Html x -> Html x
headerClass = H.headerClass . JSS.textToJSString
{-# INLINE headerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footerClass :: Text -> Html x -> Html x
footerClass = H.footerClass . JSS.textToJSString
{-# INLINE footerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
buttonClass :: Text -> Html x -> Html x
buttonClass = H.buttonClass . JSS.textToJSString
{-# INLINE buttonClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
formClass :: Text -> Html x -> Html x
formClass = H.formClass . JSS.textToJSString
{-# INLINE formClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
pClass :: Text -> Html x -> Html x
pClass = H.pClass . JSS.textToJSString
{-# INLINE pClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
sClass :: Text -> Html x -> Html x
sClass = H.sClass . JSS.textToJSString
{-# INLINE sClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ulClass :: Text -> Html x -> Html x
ulClass = H.ulClass . JSS.textToJSString
{-# INLINE ulClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
spanClass :: Text -> Html x -> Html x
spanClass = H.spanClass . JSS.textToJSString
{-# INLINE spanClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strongClass :: Text -> Html x -> Html x
strongClass = H.strongClass . JSS.textToJSString
{-# INLINE strongClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
liClass :: Text -> Html x -> Html x
liClass = H.liClass . JSS.textToJSString
{-# INLINE liClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1Class :: Text -> Html x -> Html x
h1Class = H.h1Class . JSS.textToJSString
{-# INLINE h1Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2Class :: Text -> Html x -> Html x
h2Class = H.h2Class . JSS.textToJSString
{-# INLINE h2Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3Class :: Text -> Html x -> Html x
h3Class = H.h3Class . JSS.textToJSString
{-# INLINE h3Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4Class :: Text -> Html x -> Html x
h4Class = H.h4Class . JSS.textToJSString
{-# INLINE h4Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5Class :: Text -> Html x -> Html x
h5Class = H.h5Class . JSS.textToJSString
{-# INLINE h5Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6Class :: Text -> Html x -> Html x
h6Class = H.h6Class . JSS.textToJSString
{-# INLINE h6Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hrClass :: Text -> Html x -> Html x
hrClass = H.hrClass . JSS.textToJSString
{-# INLINE hrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
preClass :: Text -> Html x -> Html x
preClass = H.preClass . JSS.textToJSString
{-# INLINE preClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
inputClass :: Text -> Html x -> Html x
inputClass = H.inputClass . JSS.textToJSString
{-# INLINE inputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
labelClass :: Text -> Html x -> Html x
labelClass = H.labelClass . JSS.textToJSString
{-# INLINE labelClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
aClass :: Text -> Html x -> Html x
aClass = H.aClass . JSS.textToJSString
{-# INLINE aClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
markClass :: Text -> Html x -> Html x
markClass = H.markClass . JSS.textToJSString
{-# INLINE markClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
rubyClass :: Text -> Html x -> Html x
rubyClass = H.rubyClass . JSS.textToJSString
{-# INLINE rubyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rtClass :: Text -> Html x -> Html x
rtClass = H.rtClass . JSS.textToJSString
{-# INLINE rtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rpClass :: Text -> Html x -> Html x
rpClass = H.rpClass . JSS.textToJSString
{-# INLINE rpClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdiClass :: Text -> Html x -> Html x
bdiClass = H.bdiClass . JSS.textToJSString
{-# INLINE bdiClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdoClass :: Text -> Html x -> Html x
bdoClass = H.bdoClass . JSS.textToJSString
{-# INLINE bdoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbrClass :: Text -> Html x -> Html x
wbrClass = H.wbrClass . JSS.textToJSString
{-# INLINE wbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
detailsClass :: Text -> Html x -> Html x
detailsClass = H.detailsClass . JSS.textToJSString
{-# INLINE detailsClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summaryClass :: Text -> Html x -> Html x
summaryClass = H.summaryClass . JSS.textToJSString
{-# INLINE summaryClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitemClass :: Text -> Html x -> Html x
menuitemClass = H.menuitemClass . JSS.textToJSString
{-# INLINE menuitemClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menuClass :: Text -> Html x -> Html x
menuClass = H.menuClass . JSS.textToJSString
{-# INLINE menuClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldsetClass :: Text -> Html x -> Html x
fieldsetClass = H.fieldsetClass . JSS.textToJSString
{-# INLINE fieldsetClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legendClass :: Text -> Html x -> Html x
legendClass = H.legendClass . JSS.textToJSString
{-# INLINE legendClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalistClass :: Text -> Html x -> Html x
datalistClass = H.datalistClass . JSS.textToJSString
{-# INLINE datalistClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroupClass :: Text -> Html x -> Html x
optgroupClass = H.optgroupClass . JSS.textToJSString
{-# INLINE optgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygenClass :: Text -> Html x -> Html x
keygenClass = H.keygenClass . JSS.textToJSString
{-# INLINE keygenClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
outputClass :: Text -> Html x -> Html x
outputClass = H.outputClass . JSS.textToJSString
{-# INLINE outputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progressClass :: Text -> Html x -> Html x
progressClass = H.progressClass . JSS.textToJSString
{-# INLINE progressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meterClass :: Text -> Html x -> Html x
meterClass = H.meterClass . JSS.textToJSString
{-# INLINE meterClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
centerClass :: Text -> Html x -> Html x
centerClass = H.centerClass . JSS.textToJSString
{-# INLINE centerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audioClass :: Text -> Html x -> Html x
audioClass = H.audioClass . JSS.textToJSString
{-# INLINE audioClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
videoClass :: Text -> Html x -> Html x
videoClass = H.videoClass . JSS.textToJSString
{-# INLINE videoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
sourceClass :: Text -> Html x -> Html x
sourceClass = H.sourceClass . JSS.textToJSString
{-# INLINE sourceClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
trackClass :: Text -> Html x -> Html x
trackClass = H.trackClass . JSS.textToJSString
{-# INLINE trackClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embedClass :: Text -> Html x -> Html x
embedClass = H.embedClass . JSS.textToJSString
{-# INLINE embedClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
objectClass :: Text -> Html x -> Html x
objectClass = H.objectClass . JSS.textToJSString
{-# INLINE objectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
paramClass :: Text -> Html x -> Html x
paramClass = H.paramClass . JSS.textToJSString
{-# INLINE paramClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
insClass :: Text -> Html x -> Html x
insClass = H.insClass . JSS.textToJSString
{-# INLINE insClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
delClass :: Text -> Html x -> Html x
delClass = H.delClass . JSS.textToJSString
{-# INLINE delClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
smallClass :: Text -> Html x -> Html x
smallClass = H.smallClass . JSS.textToJSString
{-# INLINE smallClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
citeClass :: Text -> Html x -> Html x
citeClass = H.citeClass . JSS.textToJSString
{-# INLINE citeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfnClass :: Text -> Html x -> Html x
dfnClass = H.dfnClass . JSS.textToJSString
{-# INLINE dfnClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbrClass :: Text -> Html x -> Html x
abbrClass = H.abbrClass . JSS.textToJSString
{-# INLINE abbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
timeClass :: Text -> Html x -> Html x
timeClass = H.timeClass . JSS.textToJSString
{-# INLINE timeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
varClass :: Text -> Html x -> Html x
varClass = H.varClass . JSS.textToJSString
{-# INLINE varClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
sampClass :: Text -> Html x -> Html x
sampClass = H.sampClass . JSS.textToJSString
{-# INLINE sampClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbdClass :: Text -> Html x -> Html x
kbdClass = H.kbdClass . JSS.textToJSString
{-# INLINE kbdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
captionClass :: Text -> Html x -> Html x
captionClass = H.captionClass . JSS.textToJSString
{-# INLINE captionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroupClass :: Text -> Html x -> Html x
colgroupClass = H.colgroupClass . JSS.textToJSString
{-# INLINE colgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
colClass :: Text -> Html x -> Html x
colClass = H.colClass . JSS.textToJSString
{-# INLINE colClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
navClass :: Text -> Html x -> Html x
navClass = H.navClass . JSS.textToJSString
{-# INLINE navClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
articleClass :: Text -> Html x -> Html x
articleClass = H.articleClass . JSS.textToJSString
{-# INLINE articleClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
asideClass :: Text -> Html x -> Html x
asideClass = H.asideClass . JSS.textToJSString
{-# INLINE asideClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
addressClass :: Text -> Html x -> Html x
addressClass = H.addressClass . JSS.textToJSString
{-# INLINE addressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
mainClass :: Text -> Html x -> Html x
mainClass = H.mainClass . JSS.textToJSString
{-# INLINE mainClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
bodyClass :: Text -> Html x -> Html x
bodyClass = H.bodyClass . JSS.textToJSString
{-# INLINE bodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figureClass :: Text -> Html x -> Html x
figureClass = H.figureClass . JSS.textToJSString
{-# INLINE figureClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaptionClass :: Text -> Html x -> Html x
figcaptionClass = H.figcaptionClass . JSS.textToJSString
{-# INLINE figcaptionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dlClass :: Text -> Html x -> Html x
dlClass = H.dlClass . JSS.textToJSString
{-# INLINE dlClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dtClass :: Text -> Html x -> Html x
dtClass = H.dtClass . JSS.textToJSString
{-# INLINE dtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
ddClass :: Text -> Html x -> Html x
ddClass = H.ddClass . JSS.textToJSString
{-# INLINE ddClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
imgClass :: Text -> Html x -> Html x
imgClass = H.imgClass . JSS.textToJSString
{-# INLINE imgClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframeClass :: Text -> Html x -> Html x
iframeClass = H.iframeClass . JSS.textToJSString
{-# INLINE iframeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvasClass :: Text -> Html x -> Html x
canvasClass = H.canvasClass . JSS.textToJSString
{-# INLINE canvasClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
mathClass :: Text -> Html x -> Html x
mathClass = H.mathClass . JSS.textToJSString
{-# INLINE mathClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
selectClass :: Text -> Html x -> Html x
selectClass = H.selectClass . JSS.textToJSString
{-# INLINE selectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
optionClass :: Text -> Html x -> Html x
optionClass = H.optionClass . JSS.textToJSString
{-# INLINE optionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textareaClass :: Text -> Html x -> Html x
textareaClass = H.textareaClass . JSS.textToJSString
{-# INLINE textareaClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
subClass :: Text -> Html x -> Html x
subClass = H.subClass . JSS.textToJSString
{-# INLINE subClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
supClass :: Text -> Html x -> Html x
supClass = H.supClass . JSS.textToJSString
{-# INLINE supClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
brClass :: Text -> Html x -> Html x
brClass = H.brClass . JSS.textToJSString
{-# INLINE brClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
olClass :: Text -> Html x -> Html x
olClass = H.olClass . JSS.textToJSString
{-# INLINE olClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquoteClass :: Text -> Html x -> Html x
blockquoteClass = H.blockquoteClass . JSS.textToJSString
{-# INLINE blockquoteClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
codeClass :: Text -> Html x -> Html x
codeClass = H.codeClass . JSS.textToJSString
{-# INLINE codeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
emClass :: Text -> Html x -> Html x
emClass = H.emClass . JSS.textToJSString
{-# INLINE emClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
iClass :: Text -> Html x -> Html x
iClass = H.iClass . JSS.textToJSString
{-# INLINE iClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
bClass :: Text -> Html x -> Html x
bClass = H.bClass . JSS.textToJSString
{-# INLINE bClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
uClass :: Text -> Html x -> Html x
uClass = H.uClass . JSS.textToJSString
{-# INLINE uClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
qClass :: Text -> Html x -> Html x
qClass = H.qClass . JSS.textToJSString
{-# INLINE qClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
scriptClass :: Text -> Html x -> Html x
scriptClass = H.scriptClass . JSS.textToJSString
{-# INLINE scriptClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
linkClass :: Text -> Html x -> Html x
linkClass = H.linkClass . JSS.textToJSString
{-# INLINE linkClass #-}
