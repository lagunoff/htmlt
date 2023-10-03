
Lightweight frontend library for GHC with JavaScript backend with
focus on minimalism and simplicity

## Getting started

To follow the instructions, you would need to have [nix](https://nixos.org/download.html)
installed in your system. Alternatively,
you can choose to install manually [GHC with JavaScript
Backend](https://engineering.iog.io/2023-01-24-javascript-browser-tutorial/),
and `cabal` (comes with every [GHC](https://www.haskell.org/ghc/) installation)

How to build library and the examples:
```sh
# Clone the repository
git clone https://github.com/lagunoff/htmlt.git
cd htmlt
# Enter the nix-shell
nix-shell
# Build examples with cabal
cabal --with-ghc=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg build -f examples
```
Once `cabal build` is successful, you can find js executables in
`./dist-newstyle/build/javascript-ghcjs/ghc-9.7.20230527/htmlt-0.1.0.0/x` and run them by opening `index.html` in browser

### Minimal example

```haskell
-- Example featuring <input> element and two buttons. The input value
-- is synchronized with 'DynRef's state and can be modified by either entering a
-- number into the input or by clicking one of the two buttons
app :: Html ()
app = do
  -- First create a 'DynRef
  counterRef <- newRef @Int 0
  div_ do
    input_ [type_ "number"] do
      -- Show the value inside <input>
      dynProp "value" $ JSS.pack . show <$> fromRef counterRef
      -- Parse and update the value on each InputEvent
      on "input" $ decodeEvent intDecoder $ writeRef counterRef
    br_
    -- Decrease the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef pred
      text "-"
    -- Increase the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef succ
      text "+"
  where
    intDecoder =
      valueDecoder >=> MaybeT . pure . readMaybe . JSS.unpack

main :: IO ()
main =
  void $ attachToBody app

```
[Open the demo](https://lagunoff.github.io/htmlt/js-backend/htmlt-counter.jsexe/)

## Quick API summary
<details>
  <summary>Expand to see simplified definitions</summary>
  
```hs
-- Constructing DOM
el :: JSString -> Html a -> Html a
elns :: JSString -> JSString -> Html a -> Html a
text :: JSString -> Html ()
dynText :: Dynamic JSString -> Html ()

-- Applying attributes and properties
prop :: JSString -> v -> Html ()
dynProp :: JSString -> Dynamic v -> Html ()
attr :: JSString -> JSString -> Html ()
dynAttr :: JSString -> JSString -> Html ()
toggleClass :: JSString -> Dynamic Bool -> Html ()
toggleAttr :: JSString -> Dynamic Bool -> Html ()
dynStyle :: JSString -> Dynamic JSString -> Html ()
dynStyles :: Dynamic JSString -> Html ()
dynValue :: Dynamic JSString -> Html ()
dynClass :: Dynamic JSString -> Html ()
dynChecked :: Dynamic Bool -> Html ()
dynDisabled :: Dynamic Bool -> Html ()

-- Handling DOM events
on :: EventName -> (DOMEvent -> Step ()) -> Html ()
on_ :: EventName -> Step () -> Html ()
onOptions :: EventName -> ListenerOpts -> (DOMEvent -> Step ()) -> Html ()
onGlobalEvent :: ListenerOpts -> DOMNode -> EventName -> (DOMEvent -> Step ()) -> Html ()

-- Decoding data from DOM Events
mouseDeltaDecoder :: JSVal -> MaybeT m MouseDelta
clientXYDecoder :: JSVal -> MaybeT m (Point Int)
offsetXYDecoder :: JSVal -> MaybeT m (Point Int)
pageXYDecoder :: JSVal -> MaybeT m (Point Int)
keyModifiersDecoder :: JSVal -> MaybeT m KeyModifiers
keyCodeDecoder :: JSVal -> MaybeT m Int
keyboardEventDecoder :: JSVal -> MaybeT m KeyboardEvent
valueDecoder :: JSVal -> MaybeT m JSString
checkedDecoder :: JSVal -> MaybeT m Bool

-- DOM extras, useful helpers
unsafeHtml :: MonadIO m => JSString -> HtmlT m ()
portal :: Monad m => DOMElement -> HtmlT m a -> HtmlT m a
installFinalizer :: MonadReactive m => IO () -> m ()

-- Dynamic collections
simpleList :: Dynamic [a] -> (Int -> DynRef a -> Html ()) -> Html ()

-- Arbitrary dynamic content
dyn :: Dynamic (Html ()) -> Html ()

-- Contructing Events
newEvent :: MonadReactive m => m (Event a, Trigger a)
fmap :: (a -> b) -> Event a -> Event a
never :: Event a
updates :: Dynamic a -> Event a

-- Constructing Dynamics
constDyn :: a -> Dynamic a
fromRef :: DynRef a -> Dynamic a
fmap :: (a -> b) -> Dynamic a -> Dynamic b
(<*>) :: Dynamic (a -> b) -> Dynamic a -> Dynamic b
mapDyn :: MonadReactive m => Dynamic a -> (a -> b)-> m (Dynamic b)
mapDyn2 :: MonadReactive m => Dynamic a -> Dynamic b -> (a -> b -> c) -> m (Dynamic c)
mapDyn3 :: MonadReactive m => Dynamic a -> Dynamic b -> Dynamic c -> (a -> b -> c -> d) -> m (Dynamic d)
holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a

-- Constructing DynRefs
newRef :: MonadReactive m => a -> m (DynRef a)
lensMap :: Lens' s a -> DynRef s -> DynRef a

-- Read and write DynRefs, Dynamics
readDyn :: MonadIO m => Dynamic a -> m a
readRef :: MonadIO m => DynRef a -> m a
writeRef :: DynRef a -> a -> Step ()
modifyRef :: DynRef a -> (a -> a) -> Step ()
atomicModifyRef :: DynRef a -> (a -> (a, r)) -> Step r

-- Starting and shutting down the application
atatchOptions :: StartOpts -> Html a -> IO (a, RunningApp)
attachTo :: DOMElement -> Html a -> IO (a, RunningApp)
attachToBody :: Html a -> IO (a, RunningApp)
detach :: RunningApp -> IO ()
```

</details>

## Other examples

<table>
  <tbody>
    <tr>
      <td>Counter</td>
      <td>5.6M all.js, 3.7M all.min.js</td>
      <td><a href=./examples/counter/counter.hs target=_blank>source</a></td>
      <td>
        <a href=https://lagunoff.github.io/htmlt/js-backend/htmlt-counter.jsexe/ target=_blank>open<a> |
        <a href=https://lagunoff.github.io/htmlt/js-backend/htmlt-counter.jsexe/min.html target=_blank>open minified<a>
      </td>
    </tr>
    <tr>
      <td>TodoMVC</td>
      <td>3.1M all.js, 773K all.min.js</td>
      <td><a href=./examples/todomvc/todomvc.hs target=_blank>source</a></td>
      <td>
        <a href=https://lagunoff.github.io/htmlt/js-backend/htmlt-todomvc.jsexe/ target=_blank>open<a> |
        <a href=https://lagunoff.github.io/htmlt/js-backend/htmlt-todomvc.jsexe/min.html target=_blank>open minified<a>
      </td>
    </tr>
    <tr>
      <td>Simple Routing</td>
      <td>11M all.js, 7.6M all.min.js</td>
      <td><a href=./examples/simple-routing/simple-routing.hs target=_blank>source</a></td>
      <td>
        <a href=https://lagunoff.github.io/htmlt/js-backend/htmlt-simple-routing.jsexe/ target=_blank>open<a> |
        <a href=https://lagunoff.github.io/htmlt/js-backend/htmlt-simple-routing.jsexe/min.html target=_blank>open minified<a>
      </td>
    </tr>
  </tbody>
</table>


For comparison, here are the sizes of all.js files build with GHCJS 8.6
— 1.5M
[htmlt-counter](https://lagunoff.github.io/htmlt-counter/),
1.4M
[htmlt-todomvc](https://lagunoff.github.io/htmlt-todomvc/),
3.3M
[htmlt-simple-routing](https://lagunoff.github.io/htmlt-simple-routing/)

## Todos
 - [x] Migrate to GHC with JavaScript backend
 - [ ] More examples and documentation
 - [ ] Similar library for ReactNative

## Legacy GHCJS version
The legacy version for GHCJS 8.6 and GHCJS 8.10 can still be found in
the [ghcjs](https://github.com/lagunoff/htmlt/tree/ghcjs) branch
