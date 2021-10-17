**:fire:API is unstable and incomplete, do not use in production:fire:**

Small experimental GUI library in haskell 

## Quick API summary

### Constructing DOM
```hs
el :: Text -> Html x -> Html x
el' :: Text -> Html x -> Html (x, Node)
elns :: Text -> Text -> Html x -> Html x
text :: Text -> Html ()
dynText :: Dynamic Text -> Html ()
```

`el`, `el'` and `elns` create a new HTML element and attach it to the
root, which is given by Reader-like enviroment inside `HtmlEnv`. There
are shortcut versions (`div_`, `h1_`, `span_` etc) for the most common
HTML5 tags declared in
[Html.Element](./src/HtmlT/Element.hs)

### Applying attributes and properties

```hs
prop :: Text -> v -> Html ()
dynProp :: Text -> Dynamic v -> Html ()

attr :: Text -> Text -> Html ()
dynAttr :: Text -> Text -> Html ()
```

`prop` assignes a property (like `value` to HTMLInputElement) to a
parent element `dynProp` assignes a dynamic property (`Dynamic a`),
`attr` and `dynAttr` do the same but for html attributes instead of
properties (there is a suble
[difference](https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html)).

```hs
-- Applying properties to elements
menuWidget :: Html () 
menuWidget = 
  ul_ [class_ "menu"] do
    li_ $
      a_ [href_ "#one"] $ "One" -- Html has IsString instance 
    li_ $
      a_ [href_ "#two"] $ "Two"
    li_ $
      a_ [href_ "#three"] $ "Three"
```

### Reacting to DOM events

```hs
on :: Text -> (DOMEvent -> Html ()) -> Html ()
on_ :: Text -> Html () -> Html ()
```
Essentially `on` and `on_` call [`addEventListener`](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener) on the current root
and run the given action when DOM event fires

```hs
counterWidget :: Html ()
counterWidget = do
  counterRef <- newRef @Int 0
  div_ do
    span_ $
      dynText $ T.pack . show <$> fromRef counterRef
    button_ do
      on_ "click" $ modifyRef counterRef pred
      text "Decrease"
    button_ do
      on_ "click" $ modifyRef counterRef succ
      text "Increase"
```

## Minimal complete app

```hs
import Data.Text
import HtmlT

main :: IO ()
main = attachToBody do
  colorRef <- newRef 0
  div_ [class_ "root"] do
    h1_ do
      dynStyle "color" $ getColor <$> fromRef colorRef
      on_ "mouseenter" do modifyRef colorRef (+ 1)
      text "Hello, World!"
    el "style" do text styles
  where
    getColor n =
      colors !! (n `mod` Prelude.length colors)

colors :: [Text]
colors =
  [ "rgb(173,192,84)", "rgb(22,153,190)", "rgb(22,93,24)", "rgb(199,232,42)"
  , "rgb(235,206,57)", "rgb(225,57,149)", "rgb(255,134,157)", "rgb(231,251,35)"
  , "rgb(148,122,45)", "rgb(227,10,30)", "rgb(97,22,125)", "rgb(239,243,10)"
  , "rgb(155,247,3)", "rgb(199,31,74)", "rgb(109,198,34)", "rgb(170,52,228)"
  , "rgb(61,44,247)", "rgb(118,45,39)", "rgb(248,116,17)", "rgb(27,184,238)"
  , "rgb(117,23,222)" ]
```

## Other examples

<table>
  <tbody>
    <tr>
      <td>Hello World</td>
      <td>
	    <a href=./examples/hello/hello.hs target=_blank>source</a> |
		<a href=https://lagunoff.github.io/massaraksh-hello-world/ target=_blank>demo<a>
	  </td>
    </tr>
    <tr>
      <td>TodoMVC</td>
      <td>
	    <a href=./examples/todo/todo.hs target=_blank>source</a> |
		<a href=https://lagunoff.github.io/massaraksh-todomvc target=_blank>demo<a>
	  </td>
    </tr>
    <tr>
      <td>Simple Routing</td>
      <td>
	    <a href=./examples/simple-routing/simple-routing.hs target=_blank>source</a> |
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
 - [ ] Add benchmarks
