-- | Most essential definions for public API
module HtmlT.Base where

import Control.Exception as Exception
import Control.Monad.Reader
import Data.Coerce
import Data.Foldable
import Data.IORef
import Data.JSString.Text as JSS
import Data.Text as T hiding (index)
import GHCJS.Marshal
import JavaScript.Object as Object
import JavaScript.Object.Internal

import HtmlT.DOM
import HtmlT.Decode
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
el' :: Text -> Html a -> Html (a, DOMElement)
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
  textNode <- liftIO (createTextNode txt)
  insertNode textNode

-- | Create a TextNode with dynamic content
dynText :: Dynamic Text -> Html ()
dynText d = do
  txt <- readDyn d
  textNode <- liftIO (createTextNode txt)
  void $ subscribe (updates d) \new -> void $ liftIO do
    setTextValue textNode new
  insertNode textNode

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
  forDyn_ dyn (liftIO . setup rootEl)
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
  forDyn_ d $ liftIO . setAttribute rootEl k

-- | Attach listener to the root element. First agument is the name
-- of the DOM event to listen. Second is the callback that accepts the fired
-- DOM event object
--
-- > el "button" do
-- >   on "click" \_event -> do
-- >     liftIO $ putStrLn "Clicked!"
-- >   text "Click here"
on :: Text -> (DOMEvent -> Html ()) -> Html ()
on name f = ask >>= \HtmlEnv{..} ->
  onGlobalEvent defaultListenerOpts (nodeFromElement html_current_root) name f

-- | Same as 'on' but ignores 'DOMEvent' inside the callback
on_ :: Text -> Html () -> Html ()
on_ name = on name . const

-- | Same as 'on' but allows to specify 'ListenerOpts'
onOptions :: Text -> ListenerOpts -> (DOMEvent -> Html ()) -> Html ()
onOptions name opts f = ask >>= \HtmlEnv{..} ->
  onGlobalEvent opts (nodeFromElement html_current_root) name f

-- | Attach listener, extract data of type @a@ using specified decoder
onDecoder :: Text -> Decoder a -> (a -> Html ()) -> Html ()
onDecoder name dec = on name . withDecoder dec

-- | Attach a listener to arbitrary target, not just the current root
-- element (usually that would be @window@, @document@ or @body@
-- objects)
onGlobalEvent
  :: ListenerOpts
  -- ^ Specify whether to call @event.stopPropagation()@ and
  -- @event.preventDefault()@ on the fired event
  -> DOMNode
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
  forDyn_ dyn (liftIO . setup rootEl cs)
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
  forDyn_ dyn (liftIO . setup rootEl att)
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
  forDyn_ dyn (liftIO . setup rootEl)
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
  :: forall a. DynRef [a]
  -- ^ Some dynamic data from the above scope
  -> (Int -> DynRef a -> Html ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> Html ()
simpleList dynRef h = do
  htmlEnv <- ask
  as <- readRef dynRef
  elemEnvsRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  let
    rootEl = html_current_root htmlEnv
    reactiveEnv = html_reactive_env htmlEnv
    setup :: Int -> [a] -> [a] -> [ElemEnv a] -> IO [ElemEnv a]
    setup idx old new refs = case (refs, old, new) of
      (_, [], []) -> return []
      ([], [], x:xs) -> do
        -- New list is longer, append new elements
        finalizers <- newIORef []
        elemRef <- runReactiveEnvT reactiveEnv $ newRef x
        let
          controlledRef = elemRef
            {dynref_modifier=elemModifier idx (fromRef elemRef)
            }
          newEnv = htmlEnv
            { html_reactive_env = reactiveEnv { renv_finalizers = finalizers }
            }
        (begin, end) <- runHtmlT newEnv insertBoundaries
        runHtmlT newEnv {html_insert_before_anchor = Just end} $
          h idx controlledRef
        let itemRef = ElemEnv newEnv (dynref_modifier elemRef) begin end
        (itemRef:) <$> setup (idx + 1) [] xs []
      (r:rs, x:xs, []) -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        finalizeElems (r:rs)
        return []
      (r:rs, x:xs, y:ys) -> do
        -- Update child elements along the way
        liftIO $ sync $ ee_modifier r \_ -> y
        (r:) <$> setup (idx + 1) xs ys rs
      (_, _, _) -> do
        error "simpleList: Incoherent internal state"

    finalizeElems = traverse_ \ElemEnv{..} -> liftIO do
      removeBetween rootEl ee_begin ee_end
      removeChild rootEl ee_end
      removeChild rootEl ee_begin
      let fins = renv_finalizers $ html_reactive_env ee_html_env
      readIORef fins >>= sequence_

    elemModifier :: Int -> Dynamic a -> (a -> a) -> Reactive ()
    elemModifier i dyn f = do
      oldA <- readDyn dyn
      let
        overIx 0 (_:xs) = f oldA : xs
        overIx n (x:xs) = x : overIx (n - 1) xs
        overIx _ [] = []
      dynref_modifier dynRef (overIx i)
  ees <- liftIO $ setup 0 [] as []
  liftIO $ writeIORef elemEnvsRef ees
  addFinalizer $ readIORef elemEnvsRef >>= finalizeElems
  let updatesEv = diffEvent as (dynamic_updates $ fromRef dynRef)
  void $ subscribe updatesEv \(old, new) -> do
    eenvs <- liftIO (readIORef elemEnvsRef)
    newEenvs <- liftIO $ setup 0 old new eenvs
    liftIO $ writeIORef elemEnvsRef newEenvs


-- | First build a DOM with the widget that is currently held by the
-- given Dynamic, then rebuild it every time Dynamic's value
-- changes. Useful for SPA routing, tabbed components etc.
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
dyn :: Dynamic (Html ()) -> Html ()
dyn d = do
  htmlEnv <- ask
  childRef <- liftIO (newIORef Nothing)
  (begin, end) <- insertBoundaries
  let
    rootEl = html_current_root htmlEnv
    finalizeEnv newEnv = do
      readIORef childRef >>= \case
        Just HtmlEnv{..} -> do
          finalizers <- readIORef $ renv_finalizers html_reactive_env
          sequence_ finalizers
          writeIORef (renv_finalizers html_reactive_env) []
        Nothing -> return ()
      writeIORef childRef newEnv
    setup html = liftIO do
      finalizers <- newIORef []
      let
        newEnv = htmlEnv
          { html_reactive_env = (html_reactive_env htmlEnv)
            { renv_finalizers = finalizers }
          , html_insert_before_anchor = Just end
          }
      finalizeEnv (Just newEnv)
      removeBetween rootEl begin end
      runHtmlT newEnv html
  addFinalizer (finalizeEnv Nothing)
  forDyn_ d setup

-- | Catch exceptions thrown from event handlers
catchInteractive
  :: Html ()
  -> (SomeException -> Html ())
  -> Html ()
catchInteractive html f =
  local (\e -> e {html_catch_interactive = runHtmlT e . f}) html

-- | Run an action before the current node is detached from the DOM
addFinalizer :: (MonadIO m, HasReactiveEnv m) => IO () -> m ()
addFinalizer fin = do
  ReactiveEnv{..} <- askReactiveEnv
  finRef <- liftIO $ newIORef fin
  liftIO $ modifyIORef renv_finalizers (fin:)

-- | Attach resulting DOM to the given node instead of
-- 'html_current_root'. Might be useful for implementing modal
-- dialogs, tooltips etc. Similar to what called portals in React
-- ecosystem
portal :: MonadIO m => DOMElement -> HtmlT m a -> HtmlT m a
portal rootEl = local (\e -> e
  {html_current_root = rootEl, html_insert_before_anchor = Nothing})

-- | Parse given text as HTML and attach the resulting tree to
-- 'html_current_root'. This way you can create not only HTML but
-- anything that @innerHTML@ property can create (e.g. SVG)
--
-- > -- Create a div with an SVG image inside that shows a black
-- > -- circle
-- > div_ [] do
-- >   unsafeHtml "<svg viewBox="0 0 100 100">\
-- >     \<circle cx="50" cy="50" r="50"/>\
-- >     \</svg>"
unsafeHtml :: MonadIO m => Text -> HtmlT m ()
unsafeHtml htmlText = do
  HtmlEnv{..} <- ask
  liftIO $ insertUnsafeHtml html_current_root html_insert_before_anchor htmlText
