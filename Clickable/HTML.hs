{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
module Clickable.HTML where

import Clickable.Internal
import Clickable.Types
import Control.Monad
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
  e.ien_command $ AssignRef refId (PeekStack 0)
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

attachTo :: JSExp -> HTML a -> JSM a
attachTo rootEl contents = JSM \e -> do
  e.ien_command $ PushStack rootEl
  (r, _) <- contents.unHTML Nothing e
  e.ien_command PopStack
  pure r

attach :: HTML a -> JSM a
attach = attachTo $ Id "document" `Dot` "body"

saveStackHead :: HTML RefId
saveStackHead = HTML \s e ->
  case s of
    Nothing -> do
      refId <- newRefId.unJSM e
      e.ien_command $ AssignRef refId $ PeekStack 0
      return (refId, Just refId)
    Just saved ->
      pure (saved, s)

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

dyn :: Dynamic (HTML ()) -> HTML ()
dyn val = do
  brackets <- liftJSM insertBrackets
  scope <- liftJSM newScope
  initialVal <- liftJSM $ readDyn val
  let
    update html = do
      liftJSM $ clearBrackets brackets
      html
    exec h =
      localScope scope $ customize (Ref brackets) h
  liftJSM $ exec $ update initialVal
  liftJSM $ subscribe val \newVal -> do
    freeScope scope
    exec $ update newVal

-- | Auxilliary datatype used in 'simpleList' implementation
data ElemEnv a = ElemEnv {
  brackets :: RefId,
  state_var :: DynVar a,
  elem_scope :: ScopeId
}

-- | Display dynamic collection of widgets. NOTE: changes in `DynVar
-- a` do not automatically propagate into the larger state. See
-- `OverrideVar` and todomvc example to see one way to upstream
-- changes into the larger state.
simpleList ::
  forall a. Dynamic [a] ->
  (Int -> DynVar a -> HTML ()) ->
  HTML ()
simpleList listDyn h = liftJSM do
  internalStateRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  brackets <- insertBrackets
  let
    exec brackets' scope =
      localScope scope . customize (Ref brackets')
    exec1 brackets' = customize (Ref brackets')

    setup :: Int -> [a] -> [ElemEnv a] -> JSM [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        e <- newElem x
        exec e.brackets e.elem_scope $ h idx e.state_var
        fmap (e:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        finalizeElems True (r:rs)
        return []
      -- Update existing elements along the way
      (r:rs, y:ys) -> do
        writeVar r.state_var y
        fmap (r:) $ setup (idx + 1) ys rs
    newElem :: a -> JSM (ElemEnv a)
    newElem a = do
      elem_scope <- newScope
      localScope elem_scope do
        state_var <- newVar a
        brackets' <- insertBrackets
        return ElemEnv {elem_scope, state_var, brackets = brackets'}
    finalizeElems :: Bool -> [ElemEnv a] -> JSM ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ detachBrackets ee.brackets
      destroyScope ee.elem_scope
    updateList :: [a] -> JSM ()
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
  initialVal <- readDyn listDyn
  exec1 brackets $ liftJSM $ updateList initialVal
  subscribe listDyn $ exec1 brackets . liftJSM . updateList

insertBrackets :: JSM RefId
insertBrackets = do
  brackets <- newRefId
  jsCmd $ AssignRef brackets InsertBrackets
  pure brackets

clearBrackets :: RefId -> JSM ()
clearBrackets rid = jsCmd $ ClearBrackets $ Ref rid

detachBrackets :: RefId -> JSM ()
detachBrackets rid = jsCmd $ DetachBrackets $ Ref rid

customize :: JSExp -> HTML a -> JSM a
customize elm action = JSM \e -> do
  e.ien_command $ PushStack elm
  (r, _) <- action.unHTML Nothing e
  e.ien_command PopStack
  pure r
