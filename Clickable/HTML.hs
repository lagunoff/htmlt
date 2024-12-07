{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
module Clickable.HTML where

import Clickable.Internal
import Clickable.Types
import Control.Monad.Trans
import Data.IORef
import Data.Text (Text)

el :: Text -> HTML a -> HTML a
el tagName child = HTML \s e -> do
  e.ien_command $ PushStack $ CreateElement tagName
  (r, _) <- child.unHTML Nothing e
  e.ien_command PopIns
  pure (r, s)
{-# INLINE el #-}

elns :: Text -> Text -> HTML a -> HTML a
elns ns tagName child = HTML \s e -> do
  e.ien_command $ PushStack $ CreateElementNS ns tagName
  (r, _) <- child.unHTML s e
  e.ien_command PopIns
  pure (r, s)
{-# INLINE elns #-}

text :: Text -> HTML ()
text content = HTML \s e -> do
  e.ien_command $ PushStack $ CreateText content
  e.ien_command PopIns
  return ((), s)
{-# INLINE text #-}

dynText :: Dynamic Text -> HTML ()
dynText contentDyn = HTML \s e -> do
  c <- readDyn contentDyn
  refId <- newRefId.unJSM e
  e.ien_command $ PushStack $ CreateText c
  e.ien_command $ AssignRef e.ien_scope refId (PeekStack 0)
  e.ien_command PopIns
  let k nval = JSM \e' ->
        e'.ien_command $ UpdateText (Ref refId) nval
  (subscribe contentDyn k).unJSM e
  pure ((), s)
{-# INLINEABLE dynText #-}

property :: ToJSVal val => Text -> val -> HTML ()
property k v = HTML \s e -> do
  e.ien_command $ ElementProp (PeekStack 0) k $ toJSVal v
  pure ((), s)
{-# INLINE property #-}

dynProp :: ToJSVal val => Text -> Dynamic val -> HTML ()
dynProp propName dynVal = HTML \s e -> do
  (refId, s') <- saveStackHead.unHTML s e
  initVal <- readDyn dynVal
  e.ien_command $ ElementProp (PeekStack 0) propName $ toJSVal initVal
  let k nval = JSM \e' ->
        e'.ien_command $ ElementProp (Ref refId) propName $ toJSVal nval
  unJSM (subscribe dynVal k) e
  pure ((), s')
{-# INLINE dynProp #-}

attribute :: Text -> Text -> HTML ()
attribute k v = HTML \s e -> do
  e.ien_command $ ElementAttr (PeekStack 0) k v
  pure ((), s)
{-# INLINE attribute #-}

dynAttr :: Text -> Dynamic Text -> HTML ()
dynAttr propName dynVal = HTML \s e -> do
  (refId, s') <- saveStackHead.unHTML s e
  initVal <- readDyn dynVal
  e.ien_command $ ElementAttr (PeekStack 0) propName initVal
  let k nval = JSM \e' ->
        e'.ien_command $ ElementAttr (Ref refId) propName nval
  unJSM (subscribe dynVal k) e
  pure ((), s')
{-# INLINE dynAttr #-}

toggleClass :: Text -> Dynamic Bool -> HTML ()
toggleClass className dynEnable = HTML \s e -> do
  (refId, s') <- saveStackHead.unHTML s e
  v <- readDyn dynEnable
  let k enable = JSM \e' -> e'.ien_command
          if enable
            then ClassListAdd (Ref refId) className
            else ClassListRemove (Ref refId) className
  unJSM (k v) e
  unJSM (subscribe dynEnable k) e
  pure ((), s')
{-# INLINE toggleClass #-}

execHTML :: JSExp -> HTML a -> JSM a
execHTML elm action = JSM \e -> do
  e.ien_command $ PushStack elm
  (r, _) <- action.unHTML Nothing e
  e.ien_command PopStack
  pure r

execHTMLBody :: HTML a -> JSM a
execHTMLBody = execHTML $ Id "document" `Dot` "body"

saveStackHead :: HTML RefId
saveStackHead = HTML \s e ->
  case s of
    Nothing -> do
      refId <- newRefId.unJSM e
      e.ien_command $ AssignRef e.ien_scope refId $ PeekStack 0
      return (refId, Just refId)
    Just saved ->
      pure (saved, s)

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

dyn :: Dynamic (HTML ()) -> HTML ()
dyn val = do
  scope <- liftJSM newScope
  place <- liftJSM insertPlaceholder
  initial <- liftJSM $ readDyn val
  liftJSM $ update scope place initial
  liftJSM $ subscribe val \newVal -> do
    freeScope scope
    update scope place newVal
  where
    update scope place content =
      localScope scope do
        clearPlaceholder place
        execHTML (Ref place) content

-- | Auxilliary datatype used in 'simpleList' implementation
data InternalElem a = InternalElem {
  placeholder :: RefId,
  elem_state :: DynVar a,
  elem_scope :: ScopeId
}

-- | Display dynamic collection of widgets. NOTE: changes in `DynVar
-- a` do not automatically propagate into the larger state. See
-- `OverrideVar` and todomvc example to see one way to upstream
-- changes into the larger state.
simpleList :: forall a.
  Dynamic [a] ->
  (DynVar a -> HTML ()) ->
  HTML ()
simpleList listDyn h = do
  ref <- liftIO $ newIORef ([] :: [InternalElem a])
  place <- liftJSM insertPlaceholder
  initial <- readDyn listDyn
  liftJSM $ execHTML (Ref place) $ liftJSM $ updateList ref initial
  liftJSM $ subscribe listDyn $ execHTML (Ref place) . liftJSM . updateList ref
  where
    synchronize :: [a] -> [InternalElem a] -> JSM [InternalElem a]
    synchronize [] [] = return []
    synchronize (x:xs) [] = do
      -- New list is longer, append new elements
      ie <- newElem x
      localScope ie.elem_scope $ execHTML (Ref ie.placeholder) $ h ie.elem_state
      fmap (ie:) $ synchronize xs []
    synchronize [] (r:rs) = do
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      mapM_ dropElem (r:rs)
      pure []
    synchronize (y:ys) (r:rs) = do
      -- Update existing elements along the way
      writeVar r.elem_state y
      fmap (r:) $ synchronize ys rs
    newElem :: a -> JSM (InternalElem a)
    newElem a = do
      scope <- newScope
      localScope scope do
        elem_state <- newVar a
        place' <- insertPlaceholder
        pure InternalElem {elem_scope = scope, elem_state, placeholder = place'}
    dropElem :: InternalElem a -> JSM ()
    dropElem ie = do
      detachPlaceholder ie.placeholder
      destroyScope ie.elem_scope
    updateList :: IORef [InternalElem a] -> [a] -> JSM ()
    updateList ref new = do
      ies <- liftIO $ readIORef ref
      ies' <- synchronize new ies
      liftIO $ writeIORef ref ies'

insertPlaceholder :: JSM RefId
insertPlaceholder = JSM \e -> do
  ref <- newRefId.unJSM e
  e.ien_command $ AssignRef e.ien_scope ref InsertPlaceholder
  pure ref

clearPlaceholder :: RefId -> JSM ()
clearPlaceholder rid = jsCmd $ ClearPlaceholder $ Ref rid

detachPlaceholder :: RefId -> JSM ()
detachPlaceholder rid = jsCmd $ DetachPlaceholder $ Ref rid
