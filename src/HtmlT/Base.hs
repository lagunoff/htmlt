-- | Most basic functions and definitions exported by the library
module HtmlT.Base where

import Control.Exception
import Control.Lens hiding ((#))
import Control.Monad.Reader
import Data.Coerce
import Data.Foldable
import Data.IORef
import Data.JSString.Text as JSS
import Data.List as L
import Data.Text as T hiding (index)
import Language.Javascript.JSaddle as JS
import Debug.Trace

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
el :: Text -> HtmlT x -> HtmlT x
el tag child = do
  newRootEl <- liftJSM (createElement tag)
  withRootNode newRootEl child

-- | Same as 'el' but also returns the reference to the new element
el' :: Text -> HtmlT x -> HtmlT (x, Node)
el' tag child = do
  newRootEl <- liftJSM (createElement tag)
  (,newRootEl) <$> withRootNode newRootEl child

-- | Same as 'el' but allows to specify element's namespace, see more
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElementNS
--
-- > elns "http://www.w3.org/2000/svg" "svg" do
-- >   prop "height" "210"
-- >   prop "width" "400"
-- >   elns "http://www.w3.org/2000/svg" "path" do
-- >     prop "d" "M150 0 L75 200 L225 200 Z"
elns :: Text -> Text -> HtmlT x -> HtmlT x
elns ns tag child = do
  newRootEl <- liftJSM (createElementNS ns tag)
  withRootNode newRootEl child

-- | Create a TextNode and attach it to the root
text :: Text -> HtmlT ()
text txt = do
  textNode <- liftJSM (createTextNode txt)
  mutateRoot (`appendChild` textNode)

-- | Create a TextNode with dynamic content
dynText :: Dynamic Text -> HtmlT ()
dynText d = do
  txt <- readDyn d
  js <- askJSM
  textNode <- liftJSM (createTextNode txt)
  forEvent_ (updates d) \new -> void $ liftIO do
    flip runJSM js $ setTextValue textNode new
  mutateRoot (`appendChild` textNode)

-- | Assign a property to the root element. Don't confuse attributes
-- and properties see
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
prop :: ToJSVal v => Text -> v -> HtmlT ()
prop (JSS.textToJSString -> key) val = mutateRoot \rootEl -> do
  v <- toJSVal val
  unsafeSetProp key v (coerce rootEl)

-- | Assign a property with dynamic content to the root element
dynProp :: (ToJSVal v, FromJSVal v, Eq v) => Text -> Dynamic v -> HtmlT ()
dynProp textKey dyn = askMutateRoot >>= run where
  setup t el = toJSVal t
    >>= flip (unsafeSetProp jsKey) (coerce el)
  run mutate = void $ forDyn dyn (liftIO . mutate . setup)
  jsKey = JSS.textToJSString textKey

-- | Assign an attribute to the root element. Don't confuse attributes
-- and properties see
-- https://stackoverflow.com/questions/6003819/what-is-the-difference-between-properties-and-attributes-in-html
attr :: Text -> Text -> HtmlT ()
attr k v = mutateRoot \e -> setAttribute e k v

-- | Assign an attribute with dynamic content to the root element
dynAttr :: Text -> Dynamic Text -> HtmlT ()
dynAttr k d = do
  mutate <- askMutateRoot
  let setup v e = setAttribute e k v
  void $ forDyn d (liftIO . mutate . setup)

-- | Attach a listener to the root element. First agument is the name
-- of the DOM event to listen. Second is the callback that accepts the fired
-- DOM event object
--
-- > el "button" do
-- >   on "click" \_event -> do
-- >     liftIO $ putStrLn "Clicked!"
-- >   text "Click here"
on :: Text -> (DOMEvent -> HtmlT ()) -> HtmlT ()
on name f = ask >>= run where
  listen e rootEl = liftIO $ runHtmlT e $
    onGlobalEvent defaultListenerOpts rootEl name f
  run e = mutateRoot $ listen e

-- | Same as 'on' but ignores 'DOMEvent' inside the callback
on_ :: Text -> HtmlT () -> HtmlT ()
on_ name = on name . const

onOpts :: Text -> ListenerOpts -> (DOMEvent -> HtmlT ()) -> HtmlT ()
onOpts name opts f = ask >>= run where
  listen e rootEl = liftIO $ runHtmlT e $
    onGlobalEvent opts rootEl name f
  run e = mutateRoot $ listen e

onOpts_ :: Text -> ListenerOpts -> HtmlT () -> HtmlT ()
onOpts_ name opts = onOpts name opts . const

-- | Attach a listener to arbitrary target, not just the current root
-- element (usually that would be @window@, @document@ or @body@
-- objects)
onGlobalEvent
  :: ListenerOpts
  -- ^ Specified whether to call @event.stopPropagation()@ and
  -- @event.preventDefault()@ on the fired event
  -> Node
  -- ^ Event target
  -> Text
  -- ^ Event name
  -> (DOMEvent -> HtmlT ())
  -- ^ Callback that accepts reference to the DOM event
  -> HtmlT ()
onGlobalEvent opts target name f = ask >>= run where
  mkEvent js = Event \k -> liftIO $ flip runJSM js do
    unlisten <- addEventListener opts target name \event -> do
      void . liftIO . sync . k . f $ coerce event
    pure $ liftIO $ runJSM unlisten js
  run e@HtmlEnv{..} = void $
    subscribe (mkEvent he_js_context) (liftIO . runHtmlT e)

-- | Assign CSS classes to the current root element. Compare to @prop
-- "className"@ can be used multiple times for the same root
--
-- > el "div" do
-- >   classes "container row"
-- >   classes "mt-1 mb-2"
classes :: Text -> HtmlT ()
classes cs = mutateRoot \rootEl -> do
  for_ (T.splitOn (T.pack " ") cs) $
    classListAdd rootEl

-- | Assign a single CSS classe dynamically based on the value held by
-- the given Dynamic
--
-- > showRef <- newRef False
-- > el "div" do
-- >   toggleClass "show" $ fromRef showRef
-- > el "button" do
-- >   on_ "click" $ modifyRef showRef not
-- >   text "Toggle visibility"
toggleClass :: Text -> Dynamic Bool -> HtmlT ()
toggleClass cs dyn = askMutateRoot >>= run where
  setup cs enable rootEl = case enable of
    True  -> classListAdd rootEl cs
    False -> classListRemove rootEl cs
  run mutate = void $ forDyn dyn (liftIO . mutate . setup cs)

-- | Assign a boolean attribute dynamically based on the value held by
-- the given Dynamic
--
-- > hiddenRef <- newRef True
-- > el "div" do
-- >   toggleAttr "hidden" $ fromRef hiddenRef
-- > el "button" do
-- >   on_ "click" $ modifyRef hiddenRef not
-- >   text "Toggle visibility"
toggleAttr :: Text -> Dynamic Bool -> HtmlT ()
toggleAttr att dyn = askMutateRoot >>= run where
  setup mutate name enable rootEl = case enable of
    True -> setAttribute rootEl name (T.pack "on")
    False -> removeAttribute rootEl name
  run mutate = void $ forDyn dyn (liftIO . mutate . setup mutate att)

-- | Assign a CSS property to the root dynamically based on the value
-- held by the given Dynamic
--
-- > colorRef <- newRef True
-- > el "button" do
-- >   dynStyle "background" $ bool "initial" "red" <$> fromRef colorRef
-- >   on_ "click" $ modifyRef colorRef not
-- >   text "Toggle background color"
dynStyle :: Text -> Dynamic Text -> HtmlT ()
dynStyle cssProp dyn = askMutateRoot >>= run where
  setup t el = do
    styleVal <- unsafeGetProp "style" (coerce el)
    cssVal <- toJSVal t
    unsafeSetProp jsCssProp cssVal (coerce styleVal)
  run mutate = void $ forDyn dyn (liftIO . mutate . setup)
  jsCssProp = JSS.textToJSString cssProp

-- | Alias for @pure ()@, useful when some HtmlT action is expected.
blank :: Applicative m => m ()
blank = pure ()

-- | Attach a dynamic list to the root. Convenient for displaying
-- small dynamic collections (<100 elements). Currently has a
-- limitation — the children have to attach exactly one element to the
-- root otherwise it is possible you get runtime error after some list
-- modifications
--
-- > listRef <- newRef ["One", "Two", "Three"]
-- > el "ul" do
-- >   itraverseHtml listRef traversed \_idx elemRef -> do
-- >     el "li" $ dynText $ fromRef elemRef
-- > el "button" do
-- >   on_ "click" $ modifyRef listRef ("New Item":)
-- >   text "Append new item"
itraverseHtml
  :: forall s a
  . DynRef s
  -- ^ Some dynamic data from the above scope
  -> IndexedTraversal' Int s a
  -- ^ Point to some traversable collection inside @s@
  -> (Int -> DynRef a -> HtmlT ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> HtmlT ()
itraverseHtml dynRef l h = do
  hte <- ask
  js <- askJSM
  rootEl <- askRootNode
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
        elemRef <- runSubscribeT (he_subscriptions hte) $ newRef x
        postRef <- liftIO (newIORef [])
        let
          elemRef' = elemRef {dr_modifier=mkModifier idx (fromRef elemRef)}
          newEnv = hte
            { he_finalizers = fins
            , he_post_hooks = postRef }
          itemRef = ElemEnv newEnv elemRef' (dr_modifier elemRef)
        runHtmlT newEnv $ h idx elemRef'
        liftIO (modifyIORef' itemRefs (<> [itemRef]))
        setup s (idx + 1) [] [] xs
      (_, x:xs, []) -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        itemRefsValue <- liftIO (readIORef itemRefs)
        let (newRefs, tailRefs) = L.splitAt idx itemRefsValue
        unsub tailRefs
        childEl <- flip runJSM js $ getChildNode rootEl idx
        flip runJSM js (removeChild rootEl childEl)
        liftIO (writeIORef itemRefs newRefs)
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ sync $ ee_modifier r \_ -> y
        setup s (idx + 1) rs xs ys
      (_, _, _) -> do
        error "itraverseHtml: Incoherent internal state"

    unsub = traverse_ \ElemEnv{..} -> do
      let fins = he_finalizers ee_htmlEnv
      liftIO $ readIORef (unFinalizers fins) >>= sequence_

    mkModifier :: Int -> Dynamic a -> (a -> a) -> Reactive ()
    mkModifier idx dyn f = do
      oldA <- readDyn dyn
      dr_modifier dynRef \oldS ->
        oldS & iover l \i x -> if i == idx then f oldA else x
  liftIO $ setup s 0 [] [] (toListOf l s)
  addFinalizer $ readIORef itemRefs >>= unsub
  let eUpdates = withOld s (dynamic_updates $ fromRef dynRef)
  forEvent_ eUpdates \(old, new) -> do
    refs <- liftIO (readIORef itemRefs)
    liftIO $ setup new 0 refs (toListOf l old) (toListOf l new)
  pure ()

-- | First build a DOM with the widget that is currently held by the
-- given Dynamic, then rebuild it every time Dynamic's value
-- changes. Useful for SPA routing, tabbed components etc. Currently
-- has a limitation — 'dyn_' should be used as the only children of
-- some element
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
dyn_ :: Dynamic (HtmlT ()) -> HtmlT ()
dyn_ dyn = do
  traceM "env <- ask"
  env <- ask
  traceM "js <- askJSM"
  js <- askJSM
  traceM "childRef <- liftIO (newIORef Nothing)"
  childRef <- liftIO (newIORef Nothing)
  traceM "mutate <- askMutateRoot"
  mutate <- askMutateRoot
  let
    unsub newEnv = do
      oldEnv <- readIORef childRef
      for_ oldEnv \HtmlEnv{..} -> do
        subs <- readIORef $ unFinalizers he_finalizers
        sequence_ subs
        writeIORef (unFinalizers he_finalizers) []
      writeIORef childRef newEnv
    setup html rootEl = liftIO do
      postHooks <- newIORef []
      fins <- Finalizers <$> newIORef []
      (elmRef, flush) <- deferMutations (he_current_root env)
      let
        newEnv = env
          { he_finalizers = fins
          , he_post_hooks = postHooks
          , he_current_root = elmRef }
        triggerPost = runHtmlT newEnv . sequence_
          =<< readIORef postHooks
        commit = do
          unsub (Just newEnv)
            <* emptyContent
            <* flush
            <* triggerPost
      runHtmlT newEnv html <* commit
    emptyContent = mutate removeAllChilds
  traceM "addFinalizer (unsub Nothing)"
  addFinalizer (unsub Nothing)
  traceM "void $ forDyn dyn (liftIO . mutate . (void .) . setup)"
  void $ forDyn dyn (liftIO . mutate . (void .) . setup)

catchInteractive :: HtmlT () -> (SomeException -> HtmlT ()) -> HtmlT ()
catchInteractive html handle = ask >>= run where
  run e = local (f e) html
  f e he = he {he_catch_interactive = runHtmlT e . handle}

portal :: Node -> HtmlT a -> HtmlT a
portal rootEl h = do
  js <- askJSM
  let rootRef = NodeRef (pure rootEl) ((`runJSM` js) . ($ rootEl))
  local (\e -> e {he_current_root = rootRef}) h
