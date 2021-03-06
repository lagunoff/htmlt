-- | Most essential definions for public API
module HtmlT.Base where

import Control.Exception as Exception
import Control.Lens hiding ((#))
import Control.Monad.Reader
import Data.Coerce
import Data.Foldable
import Data.IORef
import Data.JSString.Text as JSS
import Data.List as L
import Data.Text as T hiding (index)
import GHCJS.Marshal
import JavaScript.Object as Object
import JavaScript.Object.Internal

import HtmlT.DOM
import HtmlT.Event
import HtmlT.Internal
import HtmlT.Types

-- | Create a DOM element with a given tag name and attach it to the
-- current root. Second argument contains attributes, properties and
-- children nodes for the new element
--
-- > el "div" do
-- >   prop "className" "container"
-- >   el "span" $ text "Lorem Ipsum"
el :: Text -> Html a -> Html a
el tag child = do
  newRootEl <- liftIO (createElement tag)
  appendHtmlT newRootEl child

-- | Same as 'el' but also returns the reference to the new element
el' :: Text -> Html a -> Html (a, Node)
el' tag child = do
  newRootEl <- liftIO (createElement tag)
  (,newRootEl) <$> appendHtmlT newRootEl child

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

-- | Create a TextNode and attach it to the root
text :: Text -> Html ()
text txt = do
  rootEl <- asks html_current_root
  textNode <- liftIO (createTextNode txt)
  liftIO $ appendChild rootEl textNode

-- | Create a TextNode with dynamic content
dynText :: Dynamic Text -> Html ()
dynText d = do
  txt <- readDyn d
  rootEl <- asks html_current_root
  textNode <- liftIO (createTextNode txt)
  void $ subscribe (updates d) \new -> void $ liftIO do
    setTextValue textNode new
  liftIO $ appendChild rootEl textNode

-- | Assign a property to the root element. Don't confuse attributes
-- and properties
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
prop :: ToJSVal v => Text -> v -> Html ()
prop (JSS.textToJSString -> key) val = do
  rootEl <- asks html_current_root
  v <- liftIO $ toJSVal val
  liftIO $ Object.setProp key v (coerce rootEl)

-- | Assign a property with dynamic content to the root element
dynProp
  :: (ToJSVal v, FromJSVal v, Eq v)
  => Text
  -> Dynamic v
  -> Html ()
dynProp textKey dyn = do
  rootEl <- asks html_current_root
  void $ forDyn dyn (liftIO . setup rootEl)
  where
    setup el t = toJSVal t
      >>= flip (unsafeSetProp jsKey) (coerce el)
    jsKey = JSS.textToJSString textKey

-- | Assign an attribute to the root element. Don't confuse attributes
-- and properties
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
attr :: Text -> Text -> Html ()
attr k v = asks html_current_root
  >>= \e -> liftIO (setAttribute e k v)

-- | Assign an attribute with dynamic content to the root element
dynAttr :: Text -> Dynamic Text -> Html ()
dynAttr k d = do
  rootEl <- asks html_current_root
  void $ forDyn d $ liftIO . setAttribute rootEl k

-- | Attach a listener to the root element. First agument is the name
-- of the DOM event to listen. Second is the callback that accepts the fired
-- DOM event object
--
-- > el "button" do
-- >   on "click" \_event -> do
-- >     liftIO $ putStrLn "Clicked!"
-- >   text "Click here"
on :: Text -> (DOMEvent -> Html ()) -> Html ()
on name f = ask >>= \HtmlEnv{..} ->
  onGlobalEvent defaultListenerOpts html_current_root name f

-- | Same as 'on' but ignores 'DOMEvent' inside the callback
on_ :: Text -> Html () -> Html ()
on_ name = on name . const

-- | Same as 'on' but allows to specify 'ListenerOpts'
onOptions :: Text -> ListenerOpts -> (DOMEvent -> Html ()) -> Html ()
onOptions name opts f = ask >>= \HtmlEnv{..} ->
  onGlobalEvent opts html_current_root name f

-- | Same as 'onOptions' but ignores 'DOMEvent' inside the callback
onOptions_ :: Text -> ListenerOpts -> Html () -> Html ()
onOptions_ name opts = onOptions name opts . const

-- | Attach a listener to arbitrary target, not just the current root
-- element (usually that would be @window@, @document@ or @body@
-- objects)
onGlobalEvent
  :: ListenerOpts
  -- ^ Specify whether to call @event.stopPropagation()@ and
  -- @event.preventDefault()@ on the fired event
  -> Node
  -- ^ Event target
  -> Text
  -- ^ Event name
  -> (DOMEvent -> Html ())
  -- ^ Callback that accepts reference to the DOM event
  -> Html ()
onGlobalEvent opts target name f = do
  htmlEnv <- ask
  void $ subscribe (mkEvent htmlEnv) (liftIO . runHtmlT htmlEnv)
  where
    mkEvent htmlEnv = Event \callback -> liftIO do
      unlisten <- addEventListener opts target name $
        void . liftIO . catchExs htmlEnv . sync . callback . f . coerce
      return $ liftIO unlisten
    catchExs HtmlEnv{..} = (`Exception.catch` html_catch_interactive)

-- | Assign CSS classes to the current root element. Compared to @prop
-- "className"@ can be used multiple times for the same root
--
-- > el "div" do
-- >   classes "container row"
-- >   classes "mt-1 mb-2"
classes :: Text -> Html ()
classes cs = do
  rootEl <- asks html_current_root
  for_ (T.splitOn " " cs) $ liftIO . classListAdd rootEl

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
  rootEl <- asks html_current_root
  void $ forDyn dyn (liftIO . setup rootEl cs)
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
  rootEl <- asks html_current_root
  void $ forDyn dyn (liftIO . setup rootEl att)
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
  rootEl <- asks html_current_root
  void $ forDyn dyn (liftIO . setup rootEl)
  where
    setup el t = do
      styleVal <- Object.getProp "style" (coerce el)
      cssVal <- toJSVal t
      unsafeSetProp jsCssProp cssVal (coerce styleVal)
    jsCssProp = JSS.textToJSString cssProp

-- | Alias for @pure ()@, useful when some Html action is expected.
blank :: Applicative m => m ()
blank = pure ()

-- | Attach a dynamic list to the root. Convenient for displaying
-- small dynamic collections (<100 elements). Currently has a
-- limitation — the children widgets have to has exactly one element
-- in their root level otherwise it is possible you get runtime error
-- after list modifications
--
-- > listRef <- newRef ["One", "Two", "Three"]
-- > el "ul" do
-- >   simpleList listRef traversed \_idx elemRef -> do
-- >     el "li" $ dynText $ fromRef elemRef
-- > el "button" do
-- >   on_ "click" $ modifyRef listRef ("New Item":)
-- >   text "Append new item"
simpleList
  :: forall s a
  . DynRef s
  -- ^ Some dynamic data from the above scope
  -> IndexedTraversal' Int s a
  -- ^ Point to some traversable collection inside @s@
  -> (Int -> DynRef a -> Html ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> Html ()
simpleList dynRef l h = do
  hte <- ask
  rootEl <- asks html_current_root
  s <- readRef dynRef
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ElemEnv a] -> [a] -> [a] -> IO ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])    -> pure ()
      ([], [], x:xs) -> do
        -- New list is longer, append new elements
        fins <- Finalizers <$> newIORef []
        elemRef <- runSubscribeT (html_subscriptions hte) $ newRef x
        postRef <- liftIO (newIORef [])
        let
          elemRef' = elemRef {dynref_modifier=mkModifier idx (fromRef elemRef)}
          newEnv = hte
            { html_finalizers = fins
            , html_post_hooks = postRef }
          itemRef = ElemEnv newEnv elemRef' (dynref_modifier elemRef)
        runHtmlT newEnv $ h idx elemRef'
        liftIO (modifyIORef' itemRefs (<> [itemRef]))
        setup s (idx + 1) [] [] xs
      (_, x:xs, []) -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        itemRefsValue <- liftIO (readIORef itemRefs)
        let (newRefs, tailRefs) = L.splitAt idx itemRefsValue
        unsub tailRefs
        childEl <- getChildNode rootEl idx
        removeChild rootEl childEl
        liftIO (writeIORef itemRefs newRefs)
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ sync $ ee_modifier r \_ -> y
        setup s (idx + 1) rs xs ys
      (_, _, _) -> do
        error "simpleList: Incoherent internal state"

    unsub = traverse_ \ElemEnv{..} -> do
      let fins = html_finalizers ee_html_env
      liftIO $ readIORef (unFinalizers fins) >>= sequence_

    mkModifier :: Int -> Dynamic a -> (a -> a) -> Reactive ()
    mkModifier idx dyn f = do
      oldA <- readDyn dyn
      dynref_modifier dynRef \oldS ->
        oldS & iover l \i x -> if i == idx then f oldA else x
  liftIO $ setup s 0 [] [] (toListOf l s)
  addFinalizer $ readIORef itemRefs >>= unsub
  let eUpdates = diffEvent s (dynamic_updates $ fromRef dynRef)
  void $ subscribe eUpdates \(old, new) -> do
    refs <- liftIO (readIORef itemRefs)
    liftIO $ setup new 0 refs (toListOf l old) (toListOf l new)
  pure ()

-- | First build a DOM with the widget that is currently held by the
-- given Dynamic, then rebuild it every time Dynamic's value
-- changes. Useful for SPA routing, tabbed components etc. Currently
-- has a limitation — 'dyn_' can only be used as a sole descendant of
-- its parent element (i.e. should have no siblings)
--
-- > routeRef <- newRef Home
-- > el "div"
-- >   dyn_ $ routeRef <&> \case
-- >     Home -> homeWidget
-- >     Blog -> blogWidget
-- >     Resume -> resumeWidget
-- > el "button" do
-- >   on_ "click" $ writeRef routeRef Blog
-- >   text "Show my blog page"
dyn_ :: Dynamic (Html ()) -> Html ()
dyn_ dyn = do
  env <- ask
  childRef <- liftIO (newIORef Nothing)
  let
    rootEl = html_current_root env
    unsub newEnv = do
      readIORef childRef >>= \case
        Just HtmlEnv{..} -> do
          subs <- readIORef $ unFinalizers html_finalizers
          sequence_ subs
          writeIORef (unFinalizers html_finalizers) []
        Nothing -> return ()
      writeIORef childRef newEnv
    setup rootEl html = liftIO do
      postHooks <- newIORef []
      fins <- Finalizers <$> newIORef []
      let
        newEnv = env
          { html_finalizers = fins
          , html_post_hooks = postHooks }
        commit =
          unsub (Just newEnv)
          <* removeAllChilds rootEl
          <* (readIORef postHooks >>= sequence_)
      commit *> runHtmlT newEnv html
  addFinalizer (unsub Nothing)
  void $ forDyn dyn (liftIO . setup rootEl)

-- | Catch exceptions thrown from event handlers
catchInteractive
  :: Html ()
  -> (SomeException -> Html ())
  -> Html ()
catchInteractive html f =
  local (\e -> e {html_catch_interactive = runHtmlT e . f}) html

-- | Run an action before the current node is detached from the DOM
addFinalizer :: IO () -> Html ()
addFinalizer fin = do
  fins <- askFinalizers
  finRef <- liftIO $ newIORef fin
  liftIO $ modifyIORef (unFinalizers fins) (fin:)

-- | Attach resulting DOM to the given node instead of
-- 'html_current_root'. Might be useful for implementing modals,
-- tooltips etc. Similar to what called portals in React ecosystem
portal :: Node -> Html a -> Html a
portal rootEl = local (\e -> e {html_current_root = rootEl})
