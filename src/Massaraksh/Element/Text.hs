module Massaraksh.Element.Text where

import Data.JSString.Text as JSS
import Data.Text
import Massaraksh.Types
import qualified Massaraksh.Element as H

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
divClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
divClass = H.divClass . JSS.textToJSString
{-# INLINE divClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
tableClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
tableClass = H.tableClass . JSS.textToJSString
{-# INLINE tableClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
theadClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
theadClass = H.theadClass . JSS.textToJSString
{-# INLINE theadClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbodyClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
tbodyClass = H.tbodyClass . JSS.textToJSString
{-# INLINE tbodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
trClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
trClass = H.trClass . JSS.textToJSString
{-# INLINE trClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
thClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
thClass = H.thClass . JSS.textToJSString
{-# INLINE thClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
tdClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
tdClass = H.tdClass . JSS.textToJSString
{-# INLINE tdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfootClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
tfootClass = H.tfootClass . JSS.textToJSString
{-# INLINE tfootClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
sectionClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
sectionClass = H.sectionClass . JSS.textToJSString
{-# INLINE sectionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
headerClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
headerClass = H.headerClass . JSS.textToJSString
{-# INLINE headerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footerClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
footerClass = H.footerClass . JSS.textToJSString
{-# INLINE footerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
buttonClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
buttonClass = H.buttonClass . JSS.textToJSString
{-# INLINE buttonClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
formClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
formClass = H.formClass . JSS.textToJSString
{-# INLINE formClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
pClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
pClass = H.pClass . JSS.textToJSString
{-# INLINE pClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
sClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
sClass = H.sClass . JSS.textToJSString
{-# INLINE sClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ulClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
ulClass = H.ulClass . JSS.textToJSString
{-# INLINE ulClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
spanClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
spanClass = H.spanClass . JSS.textToJSString
{-# INLINE spanClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strongClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
strongClass = H.strongClass . JSS.textToJSString
{-# INLINE strongClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
liClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
liClass = H.liClass . JSS.textToJSString
{-# INLINE liClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1Class :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
h1Class = H.h1Class . JSS.textToJSString
{-# INLINE h1Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2Class :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
h2Class = H.h2Class . JSS.textToJSString
{-# INLINE h2Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3Class :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
h3Class = H.h3Class . JSS.textToJSString
{-# INLINE h3Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4Class :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
h4Class = H.h4Class . JSS.textToJSString
{-# INLINE h4Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5Class :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
h5Class = H.h5Class . JSS.textToJSString
{-# INLINE h5Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6Class :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
h6Class = H.h6Class . JSS.textToJSString
{-# INLINE h6Class #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hrClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
hrClass = H.hrClass . JSS.textToJSString
{-# INLINE hrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
preClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
preClass = H.preClass . JSS.textToJSString
{-# INLINE preClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
inputClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
inputClass = H.inputClass . JSS.textToJSString
{-# INLINE inputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
labelClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
labelClass = H.labelClass . JSS.textToJSString
{-# INLINE labelClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
aClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
aClass = H.aClass . JSS.textToJSString
{-# INLINE aClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
markClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
markClass = H.markClass . JSS.textToJSString
{-# INLINE markClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
rubyClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
rubyClass = H.rubyClass . JSS.textToJSString
{-# INLINE rubyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rtClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
rtClass = H.rtClass . JSS.textToJSString
{-# INLINE rtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rpClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
rpClass = H.rpClass . JSS.textToJSString
{-# INLINE rpClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdiClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
bdiClass = H.bdiClass . JSS.textToJSString
{-# INLINE bdiClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdoClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
bdoClass = H.bdoClass . JSS.textToJSString
{-# INLINE bdoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbrClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
wbrClass = H.wbrClass . JSS.textToJSString
{-# INLINE wbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
detailsClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
detailsClass = H.detailsClass . JSS.textToJSString
{-# INLINE detailsClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summaryClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
summaryClass = H.summaryClass . JSS.textToJSString
{-# INLINE summaryClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitemClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
menuitemClass = H.menuitemClass . JSS.textToJSString
{-# INLINE menuitemClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menuClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
menuClass = H.menuClass . JSS.textToJSString
{-# INLINE menuClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldsetClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
fieldsetClass = H.fieldsetClass . JSS.textToJSString
{-# INLINE fieldsetClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legendClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
legendClass = H.legendClass . JSS.textToJSString
{-# INLINE legendClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalistClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
datalistClass = H.datalistClass . JSS.textToJSString
{-# INLINE datalistClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroupClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
optgroupClass = H.optgroupClass . JSS.textToJSString
{-# INLINE optgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygenClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
keygenClass = H.keygenClass . JSS.textToJSString
{-# INLINE keygenClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
outputClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
outputClass = H.outputClass . JSS.textToJSString
{-# INLINE outputClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progressClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
progressClass = H.progressClass . JSS.textToJSString
{-# INLINE progressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meterClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
meterClass = H.meterClass . JSS.textToJSString
{-# INLINE meterClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
centerClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
centerClass = H.centerClass . JSS.textToJSString
{-# INLINE centerClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audioClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
audioClass = H.audioClass . JSS.textToJSString
{-# INLINE audioClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
videoClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
videoClass = H.videoClass . JSS.textToJSString
{-# INLINE videoClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
sourceClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
sourceClass = H.sourceClass . JSS.textToJSString
{-# INLINE sourceClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
trackClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
trackClass = H.trackClass . JSS.textToJSString
{-# INLINE trackClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embedClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
embedClass = H.embedClass . JSS.textToJSString
{-# INLINE embedClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
objectClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
objectClass = H.objectClass . JSS.textToJSString
{-# INLINE objectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
paramClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
paramClass = H.paramClass . JSS.textToJSString
{-# INLINE paramClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
insClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
insClass = H.insClass . JSS.textToJSString
{-# INLINE insClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
delClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
delClass = H.delClass . JSS.textToJSString
{-# INLINE delClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
smallClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
smallClass = H.smallClass . JSS.textToJSString
{-# INLINE smallClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
citeClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
citeClass = H.citeClass . JSS.textToJSString
{-# INLINE citeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfnClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
dfnClass = H.dfnClass . JSS.textToJSString
{-# INLINE dfnClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbrClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
abbrClass = H.abbrClass . JSS.textToJSString
{-# INLINE abbrClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
timeClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
timeClass = H.timeClass . JSS.textToJSString
{-# INLINE timeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
varClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
varClass = H.varClass . JSS.textToJSString
{-# INLINE varClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
sampClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
sampClass = H.sampClass . JSS.textToJSString
{-# INLINE sampClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbdClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
kbdClass = H.kbdClass . JSS.textToJSString
{-# INLINE kbdClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
captionClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
captionClass = H.captionClass . JSS.textToJSString
{-# INLINE captionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroupClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
colgroupClass = H.colgroupClass . JSS.textToJSString
{-# INLINE colgroupClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
colClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
colClass = H.colClass . JSS.textToJSString
{-# INLINE colClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
navClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
navClass = H.navClass . JSS.textToJSString
{-# INLINE navClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
articleClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
articleClass = H.articleClass . JSS.textToJSString
{-# INLINE articleClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
asideClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
asideClass = H.asideClass . JSS.textToJSString
{-# INLINE asideClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
addressClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
addressClass = H.addressClass . JSS.textToJSString
{-# INLINE addressClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
mainClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
mainClass = H.mainClass . JSS.textToJSString
{-# INLINE mainClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
bodyClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
bodyClass = H.bodyClass . JSS.textToJSString
{-# INLINE bodyClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figureClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
figureClass = H.figureClass . JSS.textToJSString
{-# INLINE figureClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaptionClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
figcaptionClass = H.figcaptionClass . JSS.textToJSString
{-# INLINE figcaptionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dlClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
dlClass = H.dlClass . JSS.textToJSString
{-# INLINE dlClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dtClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
dtClass = H.dtClass . JSS.textToJSString
{-# INLINE dtClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
ddClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
ddClass = H.ddClass . JSS.textToJSString
{-# INLINE ddClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
imgClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
imgClass = H.imgClass . JSS.textToJSString
{-# INLINE imgClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframeClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
iframeClass = H.iframeClass . JSS.textToJSString
{-# INLINE iframeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvasClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
canvasClass = H.canvasClass . JSS.textToJSString
{-# INLINE canvasClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
mathClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
mathClass = H.mathClass . JSS.textToJSString
{-# INLINE mathClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
selectClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
selectClass = H.selectClass . JSS.textToJSString
{-# INLINE selectClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
optionClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
optionClass = H.optionClass . JSS.textToJSString
{-# INLINE optionClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textareaClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
textareaClass = H.textareaClass . JSS.textToJSString
{-# INLINE textareaClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
subClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
subClass = H.subClass . JSS.textToJSString
{-# INLINE subClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
supClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
supClass = H.supClass . JSS.textToJSString
{-# INLINE supClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
brClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
brClass = H.brClass . JSS.textToJSString
{-# INLINE brClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
olClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
olClass = H.olClass . JSS.textToJSString
{-# INLINE olClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquoteClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
blockquoteClass = H.blockquoteClass . JSS.textToJSString
{-# INLINE blockquoteClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
codeClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
codeClass = H.codeClass . JSS.textToJSString
{-# INLINE codeClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
emClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
emClass = H.emClass . JSS.textToJSString
{-# INLINE emClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
iClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
iClass = H.iClass . JSS.textToJSString
{-# INLINE iClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
bClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
bClass = H.bClass . JSS.textToJSString
{-# INLINE bClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
uClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
uClass = H.uClass . JSS.textToJSString
{-# INLINE uClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
qClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
qClass = H.qClass . JSS.textToJSString
{-# INLINE qClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
scriptClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
scriptClass = H.scriptClass . JSS.textToJSString
{-# INLINE scriptClass #-}

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
linkClass :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
linkClass = H.linkClass . JSS.textToJSString
{-# INLINE linkClass #-}
