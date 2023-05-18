
Lightweight frontend library for GHCJS with focus on minimalism and
simplicity

## Getting started

First you have to install [nix](https://nixos.org/download.html)
package manager to download and install the dependecies. By default
nix-shell gets GHCJS compiler from
[reflex-platform](https://github.com/reflex-frp/reflex-platform)
project. You have to add their binary caches to your `nix.conf` to
avoid building everything from sources

```
# ~/.config/nix.conf
substituters = https://nixcache.reflex-frp.org
trusted-substituters = https://nixcache.reflex-frp.org
trusted-public-keys = ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
```

Build the examples:
```sh
# Clone the repository
git clone https://github.com/lagunoff/htmlt.git
cd htmlt
# Enter the nix-shell
nix-shell
# Build examples with cabal
cabal build -fexamples --ghcjs --ghcjs-options="-j"
```
Once `cabal build` is successful you can find the js executables in
`dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/htmlt-0.1.0.0/x/` and run them opening `index.html` in browser

### Simple example

```haskell
-- Example featuring <input> element and two buttons. The input value
-- is synchronized with 'DynRef's state and can be modified by either entering a
-- number into the input or by clicking one of the two buttons
main :: IO ()
main = void $ attachToBody do
  -- First create a 'DynRef
  counterRef <- newRef @Int 0
  div_ do
    input_ do
      -- Show the value inside <input>
      dynProp "value" $ T.pack . show <$> fromRef counterRef
      -- Parse and update the value on each InputEvent
      on "input" $ decodeEvent intDecoder $ writeRef counterRef
    br_
    -- Decrease the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef pred
      text "Decrease"
    -- Increase the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef succ
      text "Increase"
  where
    intDecoder =
      valueDecoder >=> MaybeT . pure . readMaybe . T.unpack

```
[Open the demo](https://lagunoff.github.io/htmlt-counter/)

## Quick API summary

```hs
-- Constructing DOM
el :: Text -> Html a -> Html a
elns :: Text -> Text -> Html a -> Html a
text :: Text -> Html ()
dynText :: Dynamic Text -> Html ()

-- Applying attributes and properties
prop :: Text -> v -> Html ()
dynProp :: Text -> Dynamic v -> Html ()
attr :: Text -> Text -> Html ()
dynAttr :: Text -> Text -> Html ()
toggleClass :: Text -> Dynamic Bool -> Html ()
toggleAttr :: Text -> Dynamic Bool -> Html ()
dynStyle :: Text -> Dynamic Text -> Html ()
dynStyles :: Dynamic Text -> Html ()
dynValue :: Dynamic Text -> Html ()
dynClass :: Dynamic Text -> Html ()
dynChecked :: Dynamic Bool -> Html ()
dynDisabled :: Dynamic Bool -> Html ()

-- Handling DOM events
on :: EventName -> (DOMEvent -> Step ()) -> Html ()
on_ :: EventName -> Step () -> Html ()
onOptions :: EventName -> ListenerOpts -> (DOMEvent -> Step ()) -> Html ()
onDecoder :: EventName -> Decoder a -> (a -> Step ()) -> Html ()
onGlobalEvent :: ListenerOpts -> DOMNode -> EventName -> (DOMEvent -> Step ()) -> Html ()

-- Decoding data from DOM Events
mouseDeltaDecoder :: JSVal -> MaybeT m MouseDelta
clientXYDecoder :: JSVal -> MaybeT m (Point Int)
offsetXYDecoder :: JSVal -> MaybeT m (Point Int)
pageXYDecoder :: JSVal -> MaybeT m (Point Int)
keyModifiersDecoder :: JSVal -> MaybeT m KeyModifiers
keyCodeDecoder :: JSVal -> MaybeT m Int
keyboardEventDecoder :: JSVal -> MaybeT m KeyboardEvent
valueDecoder :: JSVal -> MaybeT m Text
checkedDecoder :: JSVal -> MaybeT m Bool

-- DOM extras, useful helpers
unsafeHtml :: MonadIO m => Text -> HtmlT m ()
portal :: Monad m => DOMElement -> HtmlT m a -> HtmlT m a
addFinalizer :: MonadReactive m => IO () -> m ()

-- Dynamic collections
simpleList :: Dynamic [a] -> (Int -> DynRef a -> Html ()) -> Html ()

-- Arbitrary dynamic content
dyn :: Dynamic (Html ()) -> Html ()

-- Contructing Events
newEvent :: MonadReactive m => m (Event a, Trigger a)
fmap :: (a -> b) -> Event a -> Event a
never :: Event a
updates :: Dynamic a -> Event a
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b

-- Constructing Dynamics
constDyn :: a -> Dynamic a
fromRef :: DynRef a -> Dynamic a
fmap :: (a -> b) -> Dynamic a -> Dynamic b
(<*>) :: Dynamic (a -> b) -> Dynamic a -> Dynamic b
mapDyn :: MonadReactive m => Dynamic a -> (a -> b)-> m (Dynamic b)
mapDyn2 :: MonadReactive m => Dynamic a -> Dynamic b -> (a -> b -> c) -> m (Dynamic c)
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

-- Starting and shutting down the application
atatchOptions :: StartOpts -> Html a -> IO (a, RunningApp)
attachTo :: DOMElement -> Html a -> IO (a, RunningApp)
attachToBody :: Html a -> IO (a, RunningApp)
detach :: RunningApp -> IO ()
```

## Other examples

<table>
  <tbody>
    <tr>
      <td>Counter</td>
      <td><a href=./examples/counter/counter.hs target=_blank>source</a></td>
      <td><a href=https://lagunoff.github.io/htmlt-counter/ target=_blank>demo<a></td>
    </tr>
    <tr>
      <td>TodoMVC</td>
      <td><a href=./examples/todomvc/todomvc.hs target=_blank>source</a></td>
      <td><a href=https://lagunoff.github.io/htmlt-todomvc/ target=_blank>demo<a></td>
    </tr>
    <tr>
      <td>Simple Routing</td>
      <td><a href=./examples/simple-routing/simple-routing.hs target=_blank>source</a></td>
      <td><a href=https://lagunoff.github.io/htmlt-simple-routing/ target=_blank>demo<a></td>
    </tr>
  </tbody>
</table>

## Todos
 - [x] API to display sum types
 - [x] Reduce compile time by getting rid of `ghcjs-dom` and
       `jsaddle-dom` from dependency list
 - [x] Add benchmarks
 - [ ] More examples and documentation
 - [ ] Similar library for ReactNative
