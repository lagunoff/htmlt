**:fire:API is unstable and incomplete, do not use in production:fire:**

## Explanation
Experimental GUI library in haskell based on Phil Freeman's
[idea](https://blog.functorial.com/posts/2018-03-12-You-Might-Not-Need-The-Virtual-DOM.html)
and [implementation](https://github.com/paf31/purescript-sdom)

## Quick API summary

### Constructing DOM
```hs
el :: Text -> Html x -> Html x
el' :: Text -> Html x -> Html (x, Node)
nsEl :: Text -> Text -> Html x -> Html x
text :: Text -> Html ()
dynText :: Dynamic Text -> Html ()
```

`el`, `el'` and `nsEl` create a new HTML element and attach it to the
parent, which is given by Reader-like enviroment inside `Html`. There
are shortcut versions (`div_`, `h1_`, `span_` etc) for the most common
HTML5 tags declared in
[Massaraksh.Element](./src/Massaraksh/Element.hs)

### Applying attributes and props

```hs
prop :: Text -> v -> Html ()
dynProp :: Text -> Dynamic v -> Html ()

(=:) :: Text -> Text -> Html () -- Infix of 'prop'
(~:) :: Text -> Dynamic v -> Html () -- Infix of 'dynProp'

attr :: Text -> Text -> Html ()
dynAttr :: Text -> Text -> Html ()
```

`prop` assignes a property (like `value` to HTMLInputElement) to a
parent element `dynProp` assignes a dynamic property (`Dynamic a`),
`attr` and `dynAttr` do the same but for html attributes instead of
properties (there is a suble
[difference](https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html)).

```hs
-- Example applying properties to tags
menuWidget :: Html () 
menuWidget = 
  ul_ do
    "className" =: "menu"
    li_ do
      a_ do "One"; "href" =: "#one" -- Html has IsString instance 
    li_ do
      a_ do "Two"; "href" =: "#two" -- It is possible to add props after children
    li_ do
      a_ do "Three"; "href" =: "#three"
```

### Reacting to events

```hs
on :: Text -> Decoder (Html x) -> Html ()
on_ :: Text -> Html x -> Html ()
```

`on` and `on_` essentially call `addEventListener` for the parent html
element, the differens is that `on` runs a `Decoder` (think Aeson
parser) against the DOM `Event` usually to extract some information
(like `value` from `InputEvent`), `on_` is used where no additional
information about the event is needed (clicks for example).


```hs
counterWidget :: Html ()
counterWidget = do
  (dCounter, modify) <- newDyn (0::Int)
  div_ do
    span_ $ dynText (showt <$> dCounter)
    button_ do
      "Decrease"
      on_ "click" do modify pred
    button_ do
      "Increase"
      on_ "click" do modify (+ 1)
```


## Minimal complete app

```hs
import Massaraksh
import Data.Text as T

type Model = Int

widget :: Html ()
widget = do
  (dynVar, modify) <- liftIO (newDyn 0)
  div_ do
    "className" =: "root"
    h1_ do
      "style" ~: headerStyle <$> dynVar
      on_ "mouseenter" do liftIO $ sync $ modify (+ 1)
      text "Hello, World!"
    el "style" do "type" =: "text/css"; text css

headerStyle n =
 ("color: "::Text) <> colors !! (n `mod` length colors)

colors =
  [ "rgb(173,192,84)", "rgb(22,153,190)", "rgb(22,93,24)", "rgb(199,232,42)"
  , "rgb(235,206,57)", "rgb(225,57,149)", "rgb(255,134,157)", "rgb(231,251,35)"
  , "rgb(148,122,45)", "rgb(227,10,30)", "rgb(97,22,125)", "rgb(239,243,10)"
  , "rgb(155,247,3)", "rgb(199,31,74)", "rgb(109,198,34)", "rgb(170,52,228)"
  , "rgb(61,44,247)", "rgb(118,45,39)", "rgb(248,116,17)", "rgb(27,184,238)"
  , "rgb(117,23,222)" ]


main = defaultMain view 0
```

## Other examples

<table>
  <tbody>
    <tr>
      <td>Hello World</td>
      <td>
	    <a href=./examples/hello-world/Main.hs target=_blank>source</a> |
		<a href=https://lagunoff.github.io/massaraksh-hello-world/ target=_blank>demo<a>
	  </td>
    </tr>
    <tr>
      <td>TodoMVC</td>
      <td>
	    <a href=./examples/todomvc/Main.hs target=_blank>source</a> |
		<a href=https://lagunoff.github.io/massaraksh-todomvc target=_blank>demo<a>
	  </td>
    </tr>
  </tbody>
</table>

## Todos
 - [x] API to display sum types
 - [x] Reduce compile time by getting rid of `ghcjs-dom` and
       `jsaddle-dom` from dependency list
 - [ ] Faster updates for large lists, using similar technique to
       [diffarray](https://hackage.haskell.org/package/diffarray-0.1.1/docs/Data-Array-Diff.html)
 - [ ] Bindings to non-web GUIs (e.g. GTK or ReactNative)
 - [ ] Split library into multiple packages `massaraksh, massaraksh-html, massaraksh-*, etc`
