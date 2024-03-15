{-|
Most essential public definions
-}
module HtmlT.Base where

import Control.Monad
import Control.Monad.Reader
import Data.Text
import Data.IORef
import Data.Foldable

import HtmlT.DOM
import HtmlT.Event
import HtmlT.Internal
import HtmlT.Types
import Wasm.Compat.Marshal
import Wasm.Compat.Prim

-- | Create a DOM element with a given tag name and attach it to
-- 'html_current_element'. Attributes, properties and children nodes can
-- be added from inside the second argument
--
-- > el "div" do
-- >   prop "className" "container"
-- >   el "span" $ text "Lorem Ipsum"
el :: Text -> Html a -> Html a
el tag child = do
  newRootEl <- liftIO (createElement tag)
  appendHtmlT newRootEl child

-- | Same as 'el' but allows to specify element's namespace
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElementNS
--
-- > elns "http://www.w3.org/2000/svg" "svg" do
-- >   prop "height" "210"
-- >   prop "width" "400"
-- >   elns "http://www.w3.org/2000/svg" "path" do
-- >     prop "d" "M150 0 L75 200 L225 200 Z"
elns :: Text -> Text -> Html a -> Html a
elns ns tag child = do
  newRootEl <- liftIO (createElementNS ns tag)
  appendHtmlT newRootEl child

-- | Create a TextNode and attach it to 'html_current_element'
text :: Text -> Html ()
text txt = do
  textNode <- liftIO (createTextNode txt)
  insertNode textNode

-- | Create a TextNode with dynamic content
dynText :: Dynamic Text -> Html ()
dynText d = do
  txt <- readDyn d
  textNode <- liftIO (createTextNode txt)
  lift $ d.updates.subscribe \new -> void $ liftIO do
    setTextValue textNode new
  insertNode textNode

-- | Assign a property to 'html_current_element'. Don't confuse
-- attributes and properties
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
prop :: ToJSVal v => Text -> v -> Html ()
prop key val = do
  rootEl <- asks html_current_element
  jkey <- liftIO $ textToJSString key
  jval <- liftIO $ toJSVal val
  liftIO $ js_setProp rootEl.unDOMElement jkey jval

-- | Assign a property with dynamic content to the root element
dynProp
  :: (ToJSVal v, FromJSVal v)
  => Text
  -> Dynamic v
  -> Html ()
dynProp key dyn = do
  jKey <- liftIO $ textToJSString key
  el <- asks html_current_element
  let setup el t = toJSVal t >>= js_setProp el.unDOMElement jKey
  lift $ performDyn $ liftIO . setup el <$> dyn

-- | Assign an attribute to the root element. Don't confuse attributes
-- and properties
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
attr :: Text -> Text -> Html ()
attr k v = do
  el <- asks html_current_element
  liftIO $ setAttribute el k v

-- | Assign an attribute with dynamic content to the root element
dynAttr :: Text -> Dynamic Text -> Html ()
dynAttr k d = do
  el <- asks html_current_element
  lift $ performDyn $ liftIO . setAttribute el k <$> d

-- | Assign CSS classes to the current root element. Compared to @prop
-- "className"@ can be used multiple times for the same root
--
-- > el "div" do
-- >   classes "container row"
-- >   classes "mt-1 mb-2"
-- classes :: Text -> Html ()
-- classes cs = do
--   rootEl <- asks html_current_element
--   for_ (T.splitOn " " cs) $ liftIO . classListAdd rootEl

-- | Assign a single CSS class dynamically based on the value held by
-- the given Dynamic
--
-- > showRef <- newRef False
-- > el "div" do
-- >   toggleClass "show" $ fromRef showRef
-- > el "button" do
-- >   on_ "click" $ modifyRef showRef not
-- >   text "Toggle visibility"
toggleClass :: Text -> Dynamic Bool -> Html ()
toggleClass cs dyn = do
  rootEl <- asks html_current_element
  lift $ performDyn $ liftIO . setup rootEl cs <$> dyn
  where
    setup rootEl cs = \case
      True -> classListAdd rootEl cs
      False -> classListRemove rootEl cs

-- | Assign a boolean attribute dynamically based on the value held by
-- the given Dynamic
--
-- > hiddenRef <- newRef True
-- > el "div" do
-- >   toggleAttr "hidden" $ fromRef hiddenRef
-- > el "button" do
-- >   on_ "click" $ modifyRef hiddenRef not
-- >   text "Toggle visibility"
toggleAttr :: Text -> Dynamic Bool -> Html ()
toggleAttr att dyn = do
  rootEl <- asks html_current_element
  lift $ performDyn $ liftIO . setup rootEl att <$> dyn
  where
    setup rootEl name = \case
      True -> setAttribute rootEl name "on"
      False -> removeAttribute rootEl name

-- | Assign a CSS property to the root dynamically based on the value
-- held by the given Dynamic
--
-- > colorRef <- newRef True
-- > el "button" do
-- >   dynStyle "background" $ bool "initial" "red" <$> fromRef colorRef
-- >   on_ "click" $ modifyRef colorRef not
-- >   text "Toggle background color"
dynStyle :: Text -> Dynamic Text -> Html ()
dynStyle cssProp dyn = do
  rootEl <- asks html_current_element
  jCssProp <- liftIO $ textToJSString cssProp
  lift $ performDyn $ liftIO . setup jCssProp rootEl <$> dyn
  where
    setup jCssProp el t = do
      styleVal <- js_getProp el.unDOMElement $ toJSString "style"
      cssVal <- toJSVal t
      js_setProp styleVal jCssProp cssVal

-- | Alias for @pure ()@, useful when some Html action is expected.
blank :: Applicative m => m ()
blank = pure ()

-- | Attach a dynamic list to the root. Convenient for displaying
-- small dynamic collections (<100 elements).
--
-- > listRef <- newRef ["One", "Two", "Three"]
-- > el "ul" do
-- >   simpleList listRef \_idx elemRef -> do
-- >     el "li" $ dynText $ fromRef elemRef
-- > el "button" do
-- >   on_ "click" $ modifyRef listRef ("New Item":)
-- >   text "Append new item"
simpleList
  :: forall a. Dynamic [a]
  -- ^ Some dynamic data from the above scope
  -> (Int -> DynRef a -> Html ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> Html ()
simpleList listDyn h = do
  outerBoundary <- insertBoundary
  htmlEnv <- asks \h -> h {html_content_boundary = Just outerBoundary}
  outerReactiveEnv <- lift ask
  prevValue <- liftIO $ newIORef []
  elemEnvsRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  let
    exec :: ElemEnv a -> Html x -> RX x
    exec e = local (const e.ee_reactive_env) .
      execHtmlT e.ee_html_env
    setup :: Int -> [a] -> [ElemEnv a] -> RX [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        elementScope <- newReactiveScope
        elementRef <- newRef x
        elementBoundary <- execHtmlT htmlEnv insertBoundary
        let
          elementHtmlEnv = htmlEnv
            { html_content_boundary = Just elementBoundary
            }
          elementReactiveEnv = outerReactiveEnv
            { scope = elementScope
            }
        let elementEnv = ElemEnv elementHtmlEnv elementReactiveEnv elementRef
        exec elementEnv $ h idx elementRef
        fmap (elementEnv:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        finalizeElems True (r:rs)
        return []
      -- Update child elements along the way
      (r:rs, y:ys) -> do
        writeRef (ee_dyn_ref r) y
        fmap (r:) $ setup (idx + 1) ys rs
    finalizeElems remove = traverse_ \ee -> do
      when remove $
        liftIO $ mapM_ removeBoundary $ ee.ee_html_env.html_content_boundary
      freeScope ee.ee_reactive_env.scope
    updateList new = do
      old <- liftIO $ atomicModifyIORef' prevValue (new,)
      eenvs <- liftIO $ readIORef elemEnvsRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef elemEnvsRef newEenvs
    cleanup = do
      old <- liftIO $ readIORef elemEnvsRef
      finalizeElems False old
  lift $ performDyn $ fmap updateList listDyn
  lift $ installFinalizer cleanup

-- | First build a DOM with the widget that is currently held by the
-- given Dynamic, then rebuild it every time Dynamic's value
-- changes. Useful for SPA routing, tabbed components etc.
--
-- > routeRef <- newRef Home
-- > el "div" do
-- >   dyn $ routeRef <&> \case
-- >     Home -> homeWidget
-- >     Blog -> blogWidget
-- >     Resume -> resumeWidget
-- > el "button" do
-- >   on_ "click" $ writeRef routeRef Blog
-- >   text "Show my blog page"
dyn :: Dynamic (Html ()) -> Html ()
dyn d = do
  htmlEnv <- ask
  boundary <- insertBoundary
  newScope <- lift newReactiveScope
  initialVal <- readDyn d
  let
    update html = do
      liftIO $ clearBoundary boundary
      html
    exec = local (\s -> s {scope = newScope}) .
      execHtmlT htmlEnv {html_content_boundary = Just boundary}
  lift $ exec $ update initialVal
  lift $ d.updates.subscribe \newVal -> do
    freeScope newScope
    exec $ update newVal

-- | Attach resulting DOM to the given node instead of
-- 'html_current_element'. Might be useful for implementing modal
-- dialogs, tooltips etc. Similar to what called portals in React
-- ecosystem
portal :: DOMElement -> Html a -> Html a
portal newRootEl html = do
  boundary <- local (\e -> e
    { html_current_element = newRootEl
    , html_content_boundary = Nothing
    }) insertBoundary
  result <- local (\e -> e
    { html_current_element = newRootEl
    , html_content_boundary = Just boundary
    }) html
  lift $ installFinalizer $ liftIO $ removeBoundary boundary
  return result

-- | Parse given text as HTML and attach the resulting tree to
-- 'html_current_element'. This way you can create not only HTML but
-- anything that @innerHTML@ property can create (e.g. SVG)
--
-- > -- Create a div with an SVG image inside that shows a black
-- > -- circle
-- > div_ [] do
-- >   unsafeHtml "<svg viewBox="0 0 100 100">\
-- >     \<circle cx="50" cy="50" r="50"/>\
-- >     \</svg>"
unsafeHtml :: Text -> Html ()
unsafeHtml htmlText = do
  henv <- ask
  let anchor = fmap (.boundary_end) henv.html_content_boundary
  liftIO $ unsafeInsertHtml henv.html_current_element anchor
    htmlText
