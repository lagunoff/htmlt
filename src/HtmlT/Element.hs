-- | Shortcuts for constructing most widely used html5 elements
module HtmlT.Element where

import Data.Text as T
import HtmlT.Base
import HtmlT.Types

-- | Used to make possible for shortcut constructors to have variable
-- length arguments. Although there is no need to pass attributes and
-- children separately because they are of the same type @HtmlT a@ the
-- layout code looks more readable with attributes passed separately
-- in the first argument. This is why shortcuts like 'div_' can have
-- one or two arguments.
--
-- Unceremoniously taken from awesome @lucid@ code see
-- https://github.com/chrisdone/lucid/blob/fb3b0e7c189c2acd8d88838d4a13923f24542ee8/src/Lucid/Base.hs#L272
class Term arg result | result -> arg where
  term
    :: Text -- ^ Name.
    -> arg -- ^ Some argument.
    -> result -- ^ Result: either an element or an attribute.

-- | Given attributes, expect more child input.
instance f ~ Html a => Term [Html ()] (f -> Html a) where
  term name attrs = el name . (sequence_ attrs *>)
  {-# INLINE term #-}

-- | Given children immediately, just use that and expect no
-- attributes.
instance Term (Html a) (Html a) where
  term = el
  {-# INLINE term #-}

div_ :: Term arg result => arg -> result
div_ = term "div"
{-# INLINE div_ #-}

table_ :: Term arg result => arg -> result
table_ = term "table"
{-# INLINE table_ #-}

thead_ :: Term arg result => arg -> result
thead_ = term "thead"
{-# INLINE thead_ #-}

tbody_ :: Term arg result => arg -> result
tbody_ = term "tbody"
{-# INLINE tbody_ #-}

tr_ :: Term arg result => arg -> result
tr_ = term "tr"
{-# INLINE tr_ #-}

th_ :: Term arg result => arg -> result
th_ = term "th"
{-# INLINE th_ #-}

td_ :: Term arg result => arg -> result
td_ = term "td"
{-# INLINE td_ #-}

tfoot_ :: Term arg result => arg -> result
tfoot_ = term "tfoot"
{-# INLINE tfoot_ #-}

section_ :: Term arg result => arg -> result
section_ = term "section"
{-# INLINE section_ #-}

header_ :: Term arg result => arg -> result
header_ = term "header"
{-# INLINE header_ #-}

footer_ :: Term arg result => arg -> result
footer_ = term "footer"
{-# INLINE footer_ #-}

button_ :: Term arg result => arg -> result
button_ = term "button"
{-# INLINE button_ #-}

form_ :: Term arg result => arg -> result
form_ = term "form"
{-# INLINE form_ #-}

p_ :: Term arg result => arg -> result
p_ = term "p"
{-# INLINE p_ #-}

s_ :: Term arg result => arg -> result
s_ = term "s"
{-# INLINE s_ #-}

ul_ :: Term arg result => arg -> result
ul_ = term "ul"
{-# INLINE ul_ #-}

span_ :: Term arg result => arg -> result
span_ = term "span"
{-# INLINE span_ #-}

strong_ :: Term arg result => arg -> result
strong_ = term "strong"
{-# INLINE strong_ #-}

li_ :: Term arg result => arg -> result
li_ = term "li"
{-# INLINE li_ #-}

h1_ :: Term arg result => arg -> result
h1_ = term "h1"
{-# INLINE h1_ #-}

h2_ :: Term arg result => arg -> result
h2_ = term "h2"
{-# INLINE h2_ #-}

h3_ :: Term arg result => arg -> result
h3_ = term "h3"
{-# INLINE h3_ #-}

h4_ :: Term arg result => arg -> result
h4_ = term "h4"
{-# INLINE h4_ #-}

h5_ :: Term arg result => arg -> result
h5_ = term "h5"
{-# INLINE h5_ #-}

h6_ :: Term arg result => arg -> result
h6_ = term "h6"
{-# INLINE h6_ #-}

hr_ :: Term arg result => arg -> result
hr_ = term "hr"
{-# INLINE hr_ #-}

pre_ :: Term arg result => arg -> result
pre_ = term "pre"
{-# INLINE pre_ #-}

input_ :: Term arg result => arg -> result
input_ = term "input"
{-# INLINE input_ #-}

label_ :: Term arg result => arg -> result
label_ = term "label"
{-# INLINE label_ #-}

a_ :: Term arg result => arg -> result
a_ = term "a"
{-# INLINE a_ #-}

details_ :: Term arg result => arg -> result
details_ = term "details"
{-# INLINE details_ #-}

summary_ :: Term arg result => arg -> result
summary_ = term "summary"
{-# INLINE summary_ #-}

menuitem_ :: Term arg result => arg -> result
menuitem_ = term "menuitem"
{-# INLINE menuitem_ #-}

menu_ :: Term arg result => arg -> result
menu_ = term "menu"
{-# INLINE menu_ #-}

fieldset_ :: Term arg result => arg -> result
fieldset_ = term "fieldset"
{-# INLINE fieldset_ #-}

legend_ :: Term arg result => arg -> result
legend_ = term "legend"
{-# INLINE legend_ #-}

datalist_ :: Term arg result => arg -> result
datalist_ = term "datalist"
{-# INLINE datalist_ #-}

optgroup_ :: Term arg result => arg -> result
optgroup_ = term "optgroup"
{-# INLINE optgroup_ #-}

progress_ :: Term arg result => arg -> result
progress_ = term "progress"
{-# INLINE progress_ #-}

meter_ :: Term arg result => arg -> result
meter_ = term "meter"
{-# INLINE meter_ #-}

center_ :: Term arg result => arg -> result
center_ = term "center"
{-# INLINE center_ #-}

audio_ :: Term arg result => arg -> result
audio_ = term "audio"
{-# INLINE audio_ #-}

video_ :: Term arg result => arg -> result
video_ = term "video"
{-# INLINE video_ #-}

source_ :: Term arg result => arg -> result
source_ = term "source"
{-# INLINE source_ #-}

track_ :: Term arg result => arg -> result
track_ = term "track"
{-# INLINE track_ #-}

embed_ :: Term arg result => arg -> result
embed_ = term "embed"
{-# INLINE embed_ #-}

object_ :: Term arg result => arg -> result
object_ = term "object"
{-# INLINE object_ #-}

param_ :: Term arg result => arg -> result
param_ = term "param"
{-# INLINE param_ #-}

ins_ :: Term arg result => arg -> result
ins_ = term "ins"
{-# INLINE ins_ #-}

del_ :: Term arg result => arg -> result
del_ = term "del"
{-# INLINE del_ #-}

small_ :: Term arg result => arg -> result
small_ = term "small"
{-# INLINE small_ #-}

cite_ :: Term arg result => arg -> result
cite_ = term "cite"
{-# INLINE cite_ #-}

dfn_ :: Term arg result => arg -> result
dfn_ = term "dfn"
{-# INLINE dfn_ #-}

abbr_ :: Term arg result => arg -> result
abbr_ = term "abbr"
{-# INLINE abbr_ #-}

time_ :: Term arg result => arg -> result
time_ = term "time"
{-# INLINE time_ #-}

var_ :: Term arg result => arg -> result
var_ = term "var"
{-# INLINE var_ #-}

samp_ :: Term arg result => arg -> result
samp_ = term "samp"
{-# INLINE samp_ #-}

kbd_ :: Term arg result => arg -> result
kbd_ = term "kbd"
{-# INLINE kbd_ #-}

caption_ :: Term arg result => arg -> result
caption_ = term "caption"
{-# INLINE caption_ #-}

colgroup_ :: Term arg result => arg -> result
colgroup_ = term "colgroup"
{-# INLINE colgroup_ #-}

col_ :: Term arg result => arg -> result
col_ = term "col"
{-# INLINE col_ #-}

nav_ :: Term arg result => arg -> result
nav_ = term "nav"
{-# INLINE nav_ #-}

article_ :: Term arg result => arg -> result
article_ = term "article"
{-# INLINE article_ #-}

aside_ :: Term arg result => arg -> result
aside_ = term "aside"
{-# INLINE aside_ #-}

address_ :: Term arg result => arg -> result
address_ = term "address"
{-# INLINE address_ #-}

main_ :: Term arg result => arg -> result
main_ = term "main"
{-# INLINE main_ #-}

body_ :: Term arg result => arg -> result
body_ = term "body"
{-# INLINE body_ #-}

figure_ :: Term arg result => arg -> result
figure_ = term "figure"
{-# INLINE figure_ #-}

figcaption_ :: Term arg result => arg -> result
figcaption_ = term "figcaption"
{-# INLINE figcaption_ #-}

dl_ :: Term arg result => arg -> result
dl_ = term "dl"
{-# INLINE dl_ #-}

dt_ :: Term arg result => arg -> result
dt_ = term "dt"
{-# INLINE dt_ #-}

dd_ :: Term arg result => arg -> result
dd_ = term "dd"
{-# INLINE dd_ #-}

img_ :: Term arg result => arg -> result
img_ = term "img"
{-# INLINE img_ #-}

iframe_ :: Term arg result => arg -> result
iframe_ = term "iframe"
{-# INLINE iframe_ #-}

canvas_ :: Term arg result => arg -> result
canvas_ = term "canvas"
{-# INLINE canvas_ #-}

math_ :: Term arg result => arg -> result
math_ = term "math"
{-# INLINE math_ #-}

select_ :: Term arg result => arg -> result
select_ = term "select"
{-# INLINE select_ #-}

option_ :: Term arg result => arg -> result
option_ = term "option"
{-# INLINE option_ #-}

textarea_ :: Term arg result => arg -> result
textarea_ = term "textarea"
{-# INLINE textarea_ #-}

sub_ :: Term arg result => arg -> result
sub_ = term "sub"
{-# INLINE sub_ #-}

sup_ :: Term arg result => arg -> result
sup_ = term "sup"
{-# INLINE sup_ #-}

br_ :: Term arg result => arg -> result
br_ = term "br"
{-# INLINE br_ #-}

ol_ :: Term arg result => arg -> result
ol_ = term "ol"
{-# INLINE ol_ #-}

blockquote_ :: Term arg result => arg -> result
blockquote_ = term "blockquote"
{-# INLINE blockquote_ #-}

code_ :: Term arg result => arg -> result
code_ = term "code"
{-# INLINE code_ #-}

em_ :: Term arg result => arg -> result
em_ = term "em"
{-# INLINE em_ #-}

i_ :: Term arg result => arg -> result
i_ = term "i"
{-# INLINE i_ #-}

b_ :: Term arg result => arg -> result
b_ = term "b"
{-# INLINE b_ #-}

u_ :: Term arg result => arg -> result
u_ = term "u"
{-# INLINE u_ #-}

q_ :: Term arg result => arg -> result
q_ = term "q"
{-# INLINE q_ #-}

script_ :: Term arg result => arg -> result
script_ = term "script"
{-# INLINE script_ #-}

link_ :: Term arg result => arg -> result
link_ = term "link"
{-# INLINE link_ #-}
