{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
module Massaraksh.Base where

import Pipes as P
import Language.Javascript.JSaddle hiding ((!))
--import Control.Monad.State

type UI widget input output m
  = m (UIResult widget input output m)

data InputMsg props msg a where
  IMUpdate :: { _impOld :: props, _impNew :: props } -> InputMsg props msg ()
  IMCustom :: msg a -> InputMsg props msg a

data OutputMsg widget props model msg a where
  OMStep :: (props -> model) -> OutputMsg widget  props model msg ()
  OMRef :: widget -> OutputMsg widget  props model msg a
  OMCustom :: msg a -> OutputMsg widget props model msg a

data UIResult widget input output m = UIResult
  { _uirWidget :: widget
  , _uirInput  :: forall a. Proxy a (input a) () X m ()
  , _uirOutput :: forall a. Proxy X () a (output a) m () }

newtype Html input output m = Html { runHtml :: UI JSVal input output m }

el :: MonadJSM m => String -> Html input output m -> Html input output m
el tag child = Html do
  el <- liftJSM $ jsg "document" # "createElement" $ tag
  pure (UIResult el (pure ()) (pure ()))

newtype Attr input output m = Attr { runAttr :: UIResult JSVal input output m -> Html input output m }

attr :: (MonadJSM m, ToJSVal val) => String -> val -> Attr input output m
attr name val = Attr \UIResult{..} -> Html do
  liftJSM $ _uirWidget # "setAttribute" $ (name, val)
  pure UIResult{..}

prop :: (MonadJSM m, ToJSVal val) => String -> val -> Attr input output m
prop name val = Attr \UIResult{..} -> Html do
  liftJSM $ _uirWidget <# name $ val
  pure UIResult{..}

-- dynProp :: (MonadJSM m, MonadState model m, ToJSVal val, HasUpdate model input) => String -> (model -> val) -> Attr input output m
-- dynProp name mkVal = Attr \UIResult{..} -> Html do
--   val <- gets mkVal
--   liftJSM $ _uirWidget <# name $ val
--   pure UIResult{..}

data Update a = Update
  { _updOld :: a
  , _updNew :: a }

class HasUpdate s a | a -> s where
  isUpdate :: a -> Maybe (Update s)

class HasOutput m a | a -> m where
  emit :: m () -> a

class Attributable input output m h | h -> input, h -> output, h -> m where
  (!) :: Monad m => h -> Attr input output m -> h

instance Attributable i o m (Html i o m) where
  (!) (Html h) (Attr a) = Html $ h >>= runHtml . a

instance Attributable i o m (Html i o m -> Html i o m) where
  (!) f (Attr a) h = Html $ runHtml (f h) >>= runHtml . a

view01
  :: (MonadJSM m) => Html i o m
view01 =
  el "div"
    ! prop "class" "dfsdfs"
    ! prop "id" "sdfsdf"
--    ! dynProp "2e24234e" (show @Int)
    $ undefined
