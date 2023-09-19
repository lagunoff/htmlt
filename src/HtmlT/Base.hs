-- | Most essential public definions
module HtmlT.Base where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.IORef
import GHC.JS.Prim
import qualified Data.Map as Map

import HtmlT.DOM
import HtmlT.Event
import HtmlT.Internal
import HtmlT.Types
import JavaScript.Compat.Marshal
import JavaScript.Compat.String (JSString(..))

-- | Create a DOM element with a given tag name and attach it to
-- 'html_current_element'. Attributes, properties and children nodes can
-- be added from inside the second argument
--
-- > el "div" do
-- >   prop "className" "container"
-- >   el "span" $ text "Lorem Ipsum"
el :: JSString -> Html a -> Html a
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
elns :: JSString -> JSString -> Html a -> Html a
elns ns tag child = do
  newRootEl <- liftIO (createElementNS ns tag)
  appendHtmlT newRootEl child

-- | Create a TextNode and attach it to 'html_current_element'
text :: JSString -> Html ()
text txt = do
  textNode <- liftIO (createTextNode txt)
  insertNode textNode

-- | Create a TextNode with dynamic content
dynText :: Dynamic JSString -> Html ()
dynText d = do
  txt <- readDyn d
  textNode <- liftIO (createTextNode txt)
  void $ subscribe (updates d) \new -> void $ liftIO do
    setTextValue textNode new
  insertNode textNode

-- | Assign a property to 'html_current_element'. Don't confuse
-- attributes and properties
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
prop :: ToJSVal v => JSString -> v -> Html ()
prop key val = do
  rootEl <- asks html_current_element
  v <- liftIO $ toJSVal val
  liftIO $ js_setProp (unDOMElement rootEl) key v

-- | Assign a property with dynamic content to the root element
dynProp
  :: (ToJSVal v, FromJSVal v)
  => JSString
  -> Dynamic v
  -> Html ()
dynProp jsKey dyn = do
  el <- asks html_current_element
  performDyn $ liftIO . setup el <$> dyn
  where
    setup el t = toJSVal t
      >>= js_setProp (unDOMElement el) jsKey

-- | Assign an attribute to the root element. Don't confuse attributes
-- and properties
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
attr :: JSString -> JSString -> Html ()
attr k v = do
  el <- asks html_current_element
  liftIO $ setAttribute el k v

-- | Assign an attribute with dynamic content to the root element
dynAttr :: JSString -> Dynamic JSString -> Html ()
dynAttr k d = do
  el <- asks html_current_element
  performDyn $ liftIO . setAttribute el k <$> d

-- | Attach listener to the root element. First agument is the name
-- of the DOM event to listen. Second is the callback that accepts the fired
-- DOM event object
--
-- > el "button" do
-- >   on "click" \_event -> do
-- >     liftIO $ putStrLn "Clicked!"
-- >   text "Click here"
on :: EventName -> (DOMEvent -> Step ()) -> Html ()
on name k = do
  el <- asks html_current_element
  onGlobalEvent defaultListenerOpts (nodeFromElement el) name k

-- | Same as 'on' but ignores 'DOMEvent' inside the callback
on_ :: EventName -> Step () -> Html ()
on_ name = on name . const

-- | Same as 'on' but allows to specify 'ListenerOpts'
onOptions :: EventName -> ListenerOpts -> (DOMEvent -> Step ()) -> Html ()
onOptions name opts k = do
  el <- asks html_current_element
  onGlobalEvent opts (nodeFromElement el) name k

decodeEvent :: (JSVal -> MaybeT Step a) -> (a -> Step ()) -> DOMEvent -> Step ()
decodeEvent dec act (DOMEvent jsevent) =
  runMaybeT (dec jsevent) >>= maybe (pure ()) act

-- | Attach a listener to arbitrary target, not just the current root
-- element (usually that would be @window@, @document@ or @body@
-- objects)
onGlobalEvent
  :: MonadReactive m
  => ListenerOpts
  -- ^ Specify whether to call @event.stopPropagation()@ and
  -- @event.preventDefault()@ on the fired event
  -> DOMNode
  -- ^ Event target
  -> EventName
  -- ^ Event name
  -> (DOMEvent -> Step ())
  -- ^ Callback that accepts reference to the DOM event
  -> m ()
onGlobalEvent opts target name f = do
  ReactiveEnv{renv_finalizers} <- askReactiveEnv
  let
    event = Event \re cb -> liftIO do
      finalizerId <- nextQueueId re
      unlisten <- addEventListener opts target name $
        dynStep . cb . f
      modifyIORef' renv_finalizers $ Map.insert
        (FinalizerQueueId finalizerId) (CustomFinalizer unlisten)
  subscribe event id

-- | Assign CSS classes to the current root element. Compared to @prop
-- "className"@ can be used multiple times for the same root
--
-- > el "div" do
-- >   classes "container row"
-- >   classes "mt-1 mb-2"
-- classes :: JSString -> Html ()
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
toggleClass :: JSString -> Dynamic Bool -> Html ()
toggleClass cs dyn = do
  rootEl <- asks html_current_element
  performDyn $ liftIO . setup rootEl cs <$> dyn
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
toggleAttr :: JSString -> Dynamic Bool -> Html ()
toggleAttr att dyn = do
  rootEl <- asks html_current_element
  performDyn $ liftIO . setup rootEl att <$> dyn
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
dynStyle :: JSString -> Dynamic JSString -> Html ()
dynStyle cssProp dyn = do
  rootEl <- asks html_current_element
  performDyn $ liftIO . setup rootEl <$> dyn
  where
    setup el t = do
      styleVal <- getProp (unDOMElement el) "style"
      cssVal <- toJSVal t
      js_setProp styleVal cssProp cssVal

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
  boundary <- insertBoundary
  htmlEnv <- asks \h -> h {html_content_boundary = Just boundary}
  prevValue <- liftIO $ newIORef []
  elemEnvsRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  let
    setup :: Int -> [a] -> [ElemEnv a] -> Step [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        finalizers <- liftIO $ newIORef Map.empty
        elementRef <- liftIO $ execReactiveT (html_reactive_env htmlEnv) $ newRef x
        boundary <- liftIO $ execHtmlT htmlEnv insertBoundary
        let
          elementEnv = htmlEnv
            { html_reactive_env = (html_reactive_env htmlEnv)
              { renv_finalizers = finalizers }
            , html_content_boundary = Just boundary
            }
        liftIO $ execHtmlT elementEnv $ h idx elementRef
        let newElem = ElemEnv elementEnv elementRef
        fmap (newElem:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        liftIO $ finalizeElems True (r:rs)
        return []
      -- Update child elements along the way
      (r:rs, y:ys) -> do
        writeRef (ee_dyn_ref r) y
        fmap (r:) $ setup (idx + 1) ys rs
    finalizeElems remove = traverse_ \ElemEnv{ee_html_env} -> do
      when remove $
        mapM_ removeBoundary $ html_content_boundary ee_html_env
      let re = html_reactive_env ee_html_env
      finalizers <- readIORef $ renv_finalizers re
      applyFinalizer re finalizers
    updateList new = do
      old <- liftIO $ atomicModifyIORef' prevValue (new,)
      eenvs <- liftIO $ readIORef elemEnvsRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef elemEnvsRef newEenvs
  performDyn $ updateList <$> listDyn
  void $ installFinalizer $ readIORef elemEnvsRef >>= finalizeElems False

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
  childRef <- liftIO (newIORef Nothing)
  boundary <- insertBoundary
  let
    finalizeEnv newEnv = do
      readIORef childRef >>= \case
        Just HtmlEnv{html_reactive_env} -> do
          finalizers <- atomicModifyIORef' (renv_finalizers html_reactive_env) (Map.empty,)
          applyFinalizer html_reactive_env finalizers
        Nothing -> return ()
      writeIORef childRef newEnv
    setup html = liftIO do
      finalizers <- newIORef Map.empty
      let
        newEnv = htmlEnv
          { html_reactive_env = (html_reactive_env htmlEnv)
            { renv_finalizers = finalizers }
          , html_content_boundary = Just boundary
          }
      finalizeEnv (Just newEnv)
      clearBoundary boundary
      execHtmlT newEnv html
  installFinalizer (finalizeEnv Nothing)
  performDyn $ setup <$> d

-- | Run an action before the current node is detached from the DOM
installFinalizer :: MonadReactive m => IO () -> m FinalizerKey
installFinalizer fin = do
  renv <- askReactiveEnv
  finalizerId <- liftIO $ nextQueueId renv
  let finalizerKey = FinalizerQueueId finalizerId
  liftIO $ modifyIORef renv.renv_finalizers $
    Map.insert finalizerKey $ CustomFinalizer fin
  return finalizerKey

-- | Attach resulting DOM to the given node instead of
-- 'html_current_element'. Might be useful for implementing modal
-- dialogs, tooltips etc. Similar to what called portals in React
-- ecosystem
portal :: MonadIO m => DOMElement -> HtmlT m a -> HtmlT m a
portal newRootEl html = do
  boundary <- local (\e -> e
    { html_current_element = newRootEl
    , html_content_boundary = Nothing
    }) insertBoundary
  result <- local (\e -> e
    { html_current_element = newRootEl
    , html_content_boundary = Just boundary
    }) html
  installFinalizer $ removeBoundary boundary
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
unsafeHtml :: MonadIO m => JSString -> HtmlT m ()
unsafeHtml htmlText = do
  henv <- ask
  let anchor = fmap boundary_end henv.html_content_boundary
  liftIO $ unsafeInsertHtml henv.html_current_element anchor
    htmlText
