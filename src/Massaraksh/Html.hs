{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Massaraksh.Html
  ( boolProp
  , boolProp_
  , stringProp
  , textProp
  , intProp
  , doubleProp
  , stringProp_
  , textProp_
  , intProp_
  , doubleProp_
  , title_
  , selected_
  , hidden_
  , value_
  , defaultValue_
  , accept_
  , acceptCharset_
  , action_
  , autocomplete_
  , autosave_
  , disabled_
  , enctype_
  , formation_
  , list_
  , maxlength_
  , minlength_
  , method_
  , multiple_
  , novalidate_
  , pattern_
  , readonly_
  , required_
  , size_
  , max_
  , min_
  , step_
  , cols_
  , rows_
  , wrap_
  , target_
  , download_
  , downloadAs_
  , hreflang_
  , media_
  , ping_
  , rel_
  , ismap_
  , usemap_
  , shape_
  , coords_
  , src_
  , height_
  , width_
  , alt_
  , autoplay_
  , controls_
  , loop_
  , preload_
  , poster_
  , default_
  , kind_
  , srclang_
  , sandbox_
  , seamless_
  , srcdoc_
  , reversed_
  , start_
  , align_
  , colspan_
  , rowspan_
  , headers_
  , scope_
  , async_
  , charset_
  , content_
  , defer_
  , httpEquiv_
  , language_
  , scoped_
  , type_
  , name_
  , href_
  , id_
  , placeholder_
  , checked_
  , autofocus_
  , class_
  , data_
  , Html
  , Html'
  , text
  , text_
  , element
  , list
  , prop
  , prop_
  , on
  , onWithOptions
    -- * Headers
  , h1_
  , h2_
  , h3_
  , h4_
  , h5_
  , h6_
    -- * Grouping Content
  , div_
  , p_
  , hr_
  , pre_
  , blockquote_
    -- * Text
  , code_
  , em_
  , span_
  , a_
  , strong_
  , i_
  , b_
  , u_
  , sub_
  , sup_
  , br_
    -- * Lists
  , ol_
  , ul_
  , li_
  , dl_
  , dt_
  , dd_
    -- * Embedded Content
  , img_
  , iframe_
  , canvas_
  , math_
  , script_
  , link_
    -- * Inputs
  , select_
  , option_
  , textarea_
  , form_
  , input_
  , button_
    -- * Sections
  , section_
  , header_
  , footer_
  , nav_
  , article_
  , aside_
  , address_
  , main_
  , body_
    -- * Figures
  , figure_
  , figcaption_
    -- * Tables
  , table_
  , caption_
  , colgroup_
  , col_
  , tbody_
  , thead_
  , tfoot_
  , tr_
  , td_
  , th_
    -- * Less common elements
  , label_
  , fieldset_
  , legend_
  , datalist_
  , optgroup_
  , keygen_
  , output_
  , progress_
  , meter_
  , center_
    -- * Audio and Video
  , audio_
  , video_
  , source_
  , track_
    -- * Embedded objects
  , embed_
  , object_
  , param_
    -- * Text edits
  , ins_
  , del_
    -- * Semantic text
  , small_
  , cite_
  , dfn_
  , abbr_
  , time_
  , var_
  , samp_
  , kbd_
  , q_
  , s_
    -- * Less common tags
  , mark_
  , ruby_
  , rt_
  , rp_
  , bdi_
  , bdo_
  , wbr_
    -- * Interactive elements
  , details_
  , summary_
  , menuitem_
  , menu_
    -- * Events
  , targetValue
  , onInput
  , Attribute (..)
  )
where

import Control.Lens (Const (..), Lens, getConst, over)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Traversable (for)
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document (createElement, createTextNode)
import GHCJS.DOM.EventM (EventName)
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Ev
import GHCJS.DOM.Node (appendChild_, setTextContent, toNode)
import GHCJS.DOM.Types
import Language.Javascript.JSaddle (setProp)
import Language.Javascript.JSaddle ((!))
import Language.Javascript.JSaddle.Native (valueToString)
import Massaraksh.Event
import Massaraksh.UI
import Unsafe.Coerce (unsafeCoerce)

type Decoder a = JSVal -> JSM (Either T.Text a)

type Html msg input output = UI Node msg input output
type Html' msg model = UI Node msg model model

newtype Attribute msg i o = Attribute
  { runAttr
      :: Store i
      -> Sink (Either msg (i -> o))
      -> HTMLElement
      -> JSM (JSM ())
  }

text :: (i -> JSString) -> Html msg i o
text f = UI $ \model _ -> do
  doc <- currentDocumentUnchecked
  content <- f <$> readLatest model
  ui <- toNode <$> createTextNode doc content
  unsubscribe <- updates model `subscribe1` \Updates {new} ->
    setTextContent ui (Just (f new))
  pure $ UIHandle ui unsubscribe

text_ :: JSString -> Html msg i o
text_ content = UI $ \_ _ -> do
  doc <- currentDocumentUnchecked
  ui <- toNode <$> createTextNode doc content
  pure $ UIHandle ui $ pure ()

element :: JSString -> [Attribute msg i o] -> [Html msg i o] -> Html msg i o
element tag attrs childs = UI $ \model sink -> do
  doc <- currentDocumentUnchecked
  el <- uncheckedCastTo HTMLElement <$> createElement doc tag
  let sinkAttr = either (sink . Yield) (sink . Step)
      applyAttr (Attribute setup) = setup model sinkAttr el
  attrFinalizers <- for attrs applyAttr
  childFinalizers <- for childs $ \ch -> do
    UIHandle ui finalizer <- unUI ch model sink
    appendChild_ el ui
    pure finalizer
  pure $ UIHandle (toNode el) (sequence_ attrFinalizers *> sequence_ childFinalizers)

list
  :: forall s t a msg
   . Lens s t [a] [a]
  -> JSString
  -> [Attribute msg s t]
  -> Html (Int -> msg) (Nested s a) a
  -> Html msg s t
list lens tag attrs child = UI $ \model sink -> do
  doc <- currentDocumentUnchecked
  el <- uncheckedCastTo HTMLElement <$> createElement doc tag
  s <- readLatest model
  let alist = getConst (lens Const s)
  childHandles <- liftIO $ newIORef []

  let sinkAttr :: Sink (Either msg (s -> t))
      sinkAttr = either (sink . Yield) (sink . Step)

      applyAttr :: Attribute msg s t -> JSM (JSM ())
      applyAttr (Attribute setup) = setup model sinkAttr el

      itemMsg
        :: Int
        -> UIMsg ui (Int -> msg) (Nested s a) a
        -> UIMsg ui msg s t
      itemMsg _  (Ref ui) = Ref ui
      itemMsg idx (Step f) = Step $ \s' -> over lens (modifyAt idx (f . Nested s')) s'
      itemMsg idx (Yield f) = Yield $ f idx

      modifyAt :: Int -> (a -> a) -> [a] -> [a]
      modifyAt _ _ [] = []
      modifyAt idx f (x:xs)
        | idx == 0 = f x : xs
        | otherwise = x : modifyAt (idx - 1) f xs

      childFinalizer = do
        handles <- liftIO $ readIORef childHandles
        for_ handles $ \(storeHandle, finalizer) -> finalizeStore storeHandle *> finalizer
        liftIO $ writeIORef childHandles []
  unsubscribe <- updates model `subscribe1` \Updates {} -> do
    pure ()
  attrFinalizers <- for attrs applyAttr
  childHandlesVal <- for (zip [0..] alist) $ \(idx, itemVal) -> do
    storeHandle <- createStore (Nested s itemVal)
    UIHandle node finalizer <- unUI child (getStore storeHandle) $ sink . itemMsg idx
    appendChild_ el node
    pure (storeHandle, finalizer)
  liftIO $ writeIORef childHandles childHandlesVal
  pure $ UIHandle (toNode el) (childFinalizer *> sequence_ attrFinalizers *> unsubscribe)

prop :: (ToJSVal val) => JSString -> (i -> val) -> Attribute msg i o
prop name f = Attribute $ \model _ el -> do
  readLatest model >>= toJSVal . f >>= \val -> setProp (toJSString name) val (unsafeCoerce el)
  finalizer <- updates model `subscribe1` \Updates {new} ->
    toJSVal (f new) >>= \val -> setProp (toJSString name) val (unsafeCoerce el)
  pure finalizer

prop_ :: (ToJSVal val) => JSString -> val -> Attribute msg i o
prop_ name val = Attribute $ \_ _ el -> do
  toJSVal val >>= \v -> setProp (toJSString name) v (unsafeCoerce el)
  pure $ pure ()

-- | Set field to `Bool` value
boolProp :: JSString -> (i -> Bool) -> Attribute msg i o
boolProp = prop

-- | Set field to `Bool` value
boolProp_ :: JSString -> Bool -> Attribute msg i o
boolProp_ = prop_

-- | Set field to `String` value
stringProp :: JSString -> (i -> JSString) -> Attribute action i o
stringProp = prop

-- | Set field to `Text` value
textProp :: JSString -> (i -> JSString) -> Attribute action i o
textProp = prop

-- | Set field to `Int` value
intProp :: JSString -> (i -> Int) -> Attribute action i o
intProp = prop
-- | Set field to `Double` value
doubleProp ::  JSString -> (i -> Double) -> Attribute action i o
doubleProp = prop

-- | Set field to `String` value
stringProp_ :: JSString -> JSString -> Attribute action i o
stringProp_ = prop_

-- | Set field to `Text` value
textProp_ :: JSString -> JSString -> Attribute action i o
textProp_ = prop_

-- | Set field to `Int` value
intProp_ :: JSString -> Int -> Attribute action i o
intProp_ = prop_
-- | Set field to `Double` value
doubleProp_ ::  JSString -> Double -> Attribute action i o
doubleProp_ = prop_

-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null items) ] [ ]
--
-- classList_ ::  [(MisoString, Bool)] -> Attribute msg i o
-- classList_ xs =
--   textPrreplop "class" $ intercalate (" " :: MisoString) [ t | (t, True) <- xs ]
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/title>
title_ ::  JSString -> Attribute msg i o
title_ = textProp_ "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/selected>
selected_ ::  Bool -> Attribute msg i o
selected_ = boolProp_ "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hidden>
hidden_ ::  Bool -> Attribute msg i o
hidden_             = boolProp_ "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/value>
value_ ::  JSString -> Attribute msg i o
value_             = textProp_ "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defaultValue>
defaultValue_ ::  JSString -> Attribute msg i o
defaultValue_      = textProp_ "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/accept>
accept_ ::  JSString -> Attribute msg i o
accept_            = textProp_ "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/acceptCharset>
acceptCharset_ ::  JSString -> Attribute msg i o
acceptCharset_     = textProp_ "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/action>
action_ ::  JSString -> Attribute msg i o
action_            = textProp_ "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autocomplete>
autocomplete_ ::  Bool -> Attribute msg i o
autocomplete_ b = textProp_ "autocomplete" (if b then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autosave>
autosave_ ::  JSString -> Attribute msg i o
autosave_          = textProp_ "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/disabled>
disabled_ ::  Bool -> Attribute msg i o
disabled_          = boolProp_ "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/enctype>
enctype_ ::  JSString -> Attribute msg i o
enctype_           = textProp_ "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/formation>
formation_ ::  JSString -> Attribute msg i o
formation_         = textProp_ "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/list>
list_ ::  JSString -> Attribute msg i o
list_              = textProp_ "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/maxlength>
maxlength_ ::  JSString -> Attribute msg i o
maxlength_         = textProp_ "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/minlength>
minlength_ ::  JSString -> Attribute msg i o
minlength_         = textProp_ "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/method>
method_ ::  JSString -> Attribute msg i o
method_            = textProp_ "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/multiple>
multiple_ ::  Bool -> Attribute msg i o
multiple_          = boolProp_ "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/novalidate>
novalidate_ ::  Bool -> Attribute msg i o
novalidate_        = boolProp_ "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/pattern>
pattern_ ::  JSString -> Attribute msg i o
pattern_           = textProp_ "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/readonly>
readonly_ ::  Bool -> Attribute msg i o
readonly_          = boolProp_ "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/required>
required_ ::  Bool -> Attribute msg i o
required_          = boolProp_ "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/size>
size_ ::  JSString -> Attribute msg i o
size_              = textProp_ "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/for>
-- for_ ::  JSString -> Attribute msg i o
-- for_               = textProp_ "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/form>
-- form_ ::  JSString -> Attribute msg i o
-- form_               = textProp_ "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/max>
max_ ::  JSString -> Attribute msg i o
max_               = textProp_ "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/min>
min_ ::  JSString -> Attribute msg i o
min_               = textProp_ "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/step>
step_ ::  JSString -> Attribute msg i o
step_              = textProp_ "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/cols>
cols_ ::  JSString -> Attribute msg i o
cols_              = textProp_ "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rows>
rows_ ::  JSString -> Attribute msg i o
rows_              = textProp_ "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/wrap>
wrap_ ::  JSString -> Attribute msg i o
wrap_              = textProp_ "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/target>
target_ ::  JSString -> Attribute msg i o
target_            = textProp_ "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/download>
download_ ::  JSString -> Attribute msg i o
download_          = textProp_ "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/downloadAs>
downloadAs_ ::  JSString -> Attribute msg i o
downloadAs_        = textProp_ "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hreflang>
hreflang_ ::  JSString -> Attribute msg i o
hreflang_          = textProp_ "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/media>
media_ ::  JSString -> Attribute msg i o
media_             = textProp_ "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ping>
ping_ ::  JSString -> Attribute msg i o
ping_              = textProp_ "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rel>
rel_ ::  JSString -> Attribute msg i o
rel_               = textProp_ "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ismap>
ismap_ ::  JSString -> Attribute msg i o
ismap_             = textProp_ "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/usemap>
usemap_ ::  JSString -> Attribute msg i o
usemap_            = textProp_ "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/shape>
shape_ ::  JSString -> Attribute msg i o
shape_             = textProp_ "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/coords>
coords_ ::  JSString -> Attribute msg i o
coords_            = textProp_ "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/src>
src_ ::  JSString -> Attribute msg i o
src_               = textProp_ "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/height>
height_ ::  JSString -> Attribute msg i o
height_            = textProp_ "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/width>
width_ ::  JSString -> Attribute msg i o
width_             = textProp_ "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/alt>
alt_ ::  JSString -> Attribute msg i o
alt_               = textProp_ "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autoplay>
autoplay_ ::  Bool -> Attribute msg i o
autoplay_          = boolProp_ "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/controls>
controls_ ::  Bool -> Attribute msg i o
controls_          = boolProp_ "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/loop>
loop_ ::  Bool -> Attribute msg i o
loop_              = boolProp_ "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/preload>
preload_ ::  JSString -> Attribute msg i o
preload_           = textProp_ "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/poster>
poster_ ::  JSString -> Attribute msg i o
poster_            = textProp_ "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/default>
default_ ::  Bool -> Attribute msg i o
default_           = boolProp_ "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/kind>
kind_ ::  JSString -> Attribute msg i o
kind_              = textProp_ "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srclang>
srclang_ ::  JSString -> Attribute msg i o
srclang_           = textProp_ "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/sandbox>
sandbox_ ::  JSString -> Attribute msg i o
sandbox_           = textProp_ "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/seamless>
seamless_ ::  JSString -> Attribute msg i o
seamless_          = textProp_ "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srcdoc>
srcdoc_ ::  JSString -> Attribute msg i o
srcdoc_            = textProp_ "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/reversed>
reversed_ ::  JSString -> Attribute msg i o
reversed_          = textProp_ "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/start>
start_ ::  JSString -> Attribute msg i o
start_             = textProp_ "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/align>
align_ ::  JSString -> Attribute msg i o
align_             = textProp_ "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/colspan>
colspan_ ::  JSString -> Attribute msg i o
colspan_           = textProp_ "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rowspan>
rowspan_ ::  JSString -> Attribute msg i o
rowspan_           = textProp_ "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/headers>
headers_ ::  JSString -> Attribute msg i o
headers_           = textProp_ "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scope>
scope_ ::  JSString -> Attribute msg i o
scope_             = textProp_ "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/async>
async_ ::  JSString -> Attribute msg i o
async_             = textProp_ "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/charset>
charset_ ::  JSString -> Attribute msg i o
charset_           = textProp_ "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/content>
content_ ::  JSString -> Attribute msg i o
content_           = textProp_ "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defer>
defer_ ::  JSString -> Attribute msg i o
defer_             = textProp_ "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/httpEquiv>
httpEquiv_ ::  JSString -> Attribute msg i o
httpEquiv_         = textProp_ "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/language>
language_ ::  JSString -> Attribute msg i o
language_          = textProp_ "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scoped>
scoped_ ::  JSString -> Attribute msg i o
scoped_            = textProp_ "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/type>
type_ ::  JSString -> Attribute msg i o
type_ = textProp_ "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/name>
name_ ::  JSString -> Attribute msg i o
name_ = textProp_ "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/href>
href_ ::  JSString -> Attribute msg i o
href_ = textProp_ "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/id>
id_ ::  JSString -> Attribute msg i o
id_ = textProp_ "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/placeholder>
placeholder_ ::  JSString -> Attribute msg i o
placeholder_ = textProp_ "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/checked>
checked_ ::  Bool -> Attribute msg i o
checked_ = boolProp_ "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autofocus>
autofocus_ ::  Bool -> Attribute msg i o
autofocus_ = boolProp_ "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  JSString -> Attribute msg i o
class_ = textProp_ "className"
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  JSString -> JSString -> Attribute msg i o
data_ k v = textProp_ ("data-" <> toJSString k) v

on :: (IsEvent e) => EventName HTMLElement e -> (i -> Either msg (i -> o)) -> Attribute msg i o
on evName makeMsg = Attribute setupEvent where
  setupEvent model sink el = EventM.on el evName $ liftJSM $ readLatest model >>= sink . makeMsg

onWithOptions
  :: (IsEvent e, ToJSVal e)
  => EventName HTMLElement e
  -> Decoder a
  -> (i -> a -> Either msg (i -> o))
  -> Attribute msg i o
onWithOptions evName decoder makeMsg = Attribute $ \store sink el -> EventM.on el evName $ ask >>= \event -> liftJSM $ do
  model <- readLatest store
  v <- decoder =<< toJSVal event
  case v of
    Left _  -> pure ()
    Right a -> sink $ makeMsg model a

targetValue :: Decoder JSString
targetValue v = do
  val <- v ! ("target" :: JSString) ! ("value" :: JSString) >>= valueToString
  pure $ Right val

onInput :: (i -> JSString -> Either msg (i -> o)) -> Attribute msg i o
onInput = onWithOptions Ev.input targetValue

-- https://github.com/dmjio/miso/blob/0576696323652ec17a921a0be8c41e82685da374/src/Miso/Html/Element.hs
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
div_  = element "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
table_  = element "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
thead_  = element "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
tbody_  = element "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
tr_  = element "tr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
th_  = element "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
td_  = element "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
tfoot_  = element "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
section_  = element "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
header_  = element "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
footer_  = element "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
button_ = element "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
form_ = element "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
p_ = element "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
s_ = element "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ul_ = element "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
span_ = element "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
strong_ = element "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
li_ = element "li"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h1_ = element "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h2_ = element "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h3_ = element "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h4_ = element "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h5_ = element "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
h6_ = element "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: [Attribute msg i o] -> Html msg i o
hr_ = flip (element "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
pre_ = element "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: [Attribute msg i o] -> Html msg i o
input_ = flip (element "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
label_ = element "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
a_ = element "a"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
mark_ = element "mark"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ruby_ = element "ruby"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
rt_ = element "rt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
rp_ = element "rp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
bdi_ = element "bdi"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
bdo_ = element "bdo"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: [Attribute msg i o] -> Html msg i o
wbr_ = flip (element "wbr") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
details_ = element "details"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
summary_ = element "summary"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
menuitem_ = element "menuitem"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
menu_ = element "menu"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
fieldset_ = element "fieldset"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
legend_ = element "legend"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
datalist_ = element "datalist"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
optgroup_ = element "optgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
keygen_ = element "keygen"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
output_ = element "output"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
progress_ = element "progress"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
meter_ = element "meter"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
center_ = element "center"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
audio_ = element "audio"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
video_ = element "video"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: [Attribute msg i o] -> Html msg i o
source_ = flip (element "source") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: [Attribute msg i o] -> Html msg i o
track_ = flip (element "track") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: [Attribute msg i o] -> Html msg i o
embed_ = flip (element "embed") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
object_ = element "object"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: [Attribute msg i o] -> Html msg i o
param_ = flip (element "param") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ins_ = element "ins"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
del_ = element "del"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
small_ = element "small"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
cite_ = element "cite"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dfn_ = element "dfn"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
abbr_ = element "abbr"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
time_ = element "time"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
var_ = element "var"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
samp_ = element "samp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
kbd_ = element "kbd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
caption_ = element "caption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
colgroup_ = element "colgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: [Attribute msg i o] -> Html msg i o
col_ = flip (element "col") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
nav_ = element "nav"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
article_ = element "article"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
aside_ = element "aside"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
address_ = element "address"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
main_ = element "main"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
body_ = element "body"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
figure_ = element "figure"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
figcaption_ = element "figcaption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dl_ = element "dl"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dt_ = element "dt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
dd_ = element "dd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: [Attribute msg i o] -> Html msg i o
img_ = flip (element "img") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
iframe_ = element "iframe"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
canvas_ = element "canvas"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
math_ = element "math"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
select_ = element "select"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
option_ = element "option"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
textarea_ = element "textarea"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
sub_ = element "sub"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
sup_ = element "sup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: [Attribute msg i o] -> Html msg i o
br_ = flip (element "br") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
ol_ = element "ol"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
blockquote_ = element "blockquote"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
code_ = element "code"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
em_ = element "em"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
i_ = element "i"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
b_ = element "b"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
u_ = element "u"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
q_ = element "q"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: [Attribute msg i o] -> [Html msg i o] -> Html msg i o
script_ = element "script"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: [Attribute msg i o] -> Html msg i o
link_ = flip (element "link") []
