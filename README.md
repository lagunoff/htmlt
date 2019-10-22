**:fire:API is unstable and incomplete, do not use in production:fire:**

## Explanation
Experimental GUI library in haskell based on Phil Freeman's
[idea](https://blog.functorial.com/posts/2018-03-12-You-Might-Not-Need-The-Virtual-DOM.html)
and [implementation](https://github.com/paf31/purescript-sdom)

## Minimal complete app

```hs
module Main where

import Massaraksh.Html
import qualified Data.Text as T
import qualified Massaraksh.Html.Dynamic as Dyn
import qualified GHCJS.DOM.GlobalEventHandlers as E

type Model = Int

view :: Html msg Model Model
view = div_ [ class_ "root" ]
  [ h1_
    [ Dyn.style_ headerStyle
    , on E.mouseEnter handleMouseEnter
    ] [ text "Hello, World!" ]
  , el "style" [ type_ "text/css" ] [ text css ]
  ] 
  where
    headerStyle n =
      "color: " <> colors !! (n `mod` length colors)

    handleMouseEnter n =
      Right (+1)
    
    colors =
      ["rgb(173, 192, 84)","rgb(22, 153, 190)","rgb(22, 93, 24)","rgb(199, 232, 42)","rgb(235, 206, 57)","rgb(225, 57, 149)","rgb(255, 134, 157)","rgb(231, 251, 35)","rgb(148, 122, 45)","rgb(227, 10, 30)","rgb(97, 22, 125)","rgb(239, 243, 10)","rgb(155, 247, 3)","rgb(199, 31, 74)","rgb(109, 198, 34)","rgb(170, 52, 228)","rgb(61, 44, 247)","rgb(118, 45, 39)","rgb(248, 116, 17)","rgb(27, 184, 238)","rgb(117, 23, 222)"]

    css = T.unwords
      [ "html, body { margin: 0; height: 100%; }"
      , ".root { width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; }"
      , ".root > h1 { font-size: 48px; margin: 0; font-family: \"Helvetica\", Arial, sans-serif; font-weight: 600; border: dashed 4px rgba(0,0,0,0.12); cursor: default; padding: 8px 16px; }"
      ]

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
	    <a href=./examples/todomvc/TodoMVC/Main.hs target=_blank>source</a> |
		<a href=https://lagunoff.github.io/massaraksh-todomvc target=_blank>demo<a>
	  </td>
    </tr>
  </tbody>
</table>

## Todos
 - [ ] API to display sum types
 - [ ] Reduce compile time by getting rid of `ghcjs-dom` and
       `jsaddle-dom` from dependency list
 - [ ] Faster updates for large lists, using similar technique to
       [diffarray](https://hackage.haskell.org/package/diffarray-0.1.1/docs/Data-Array-Diff.html)
 - [ ] Bindings to non-web GUIs (e.g. GTK or ReactNative)
 - [ ] Split library into multiple packages `massaraksh, massaraksh-html, massaraksh-*, etc`
