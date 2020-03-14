{-# LANGUAGE TupleSections #-}
module Massaraksh.Dynamic where

import Control.Lens
import Data.IORef
import GHC.Generics
import Massaraksh.Event

data Update a = Update { updOld :: a, updNew :: a }
  deriving (Show, Eq, Generic, Functor)

-- | FIXME: Name was adopted from @reflex@ library, originally this
-- type was named @Store@ this is how js frameworks call similar
-- entities. The problem is that this name conflicts with @Dynamic@
-- from base which is also used by this library
data Dynamic a = Dynamic
  { dynamicRead    :: IO a           -- ^ Read current value
  , dynamicUpdates :: Event (Update a) -- ^ Event that fires when the value changes
  }

-- | A 'Dynamic' together with a function to modify value inside
-- it. @s@ and @t@ are input and output parameters for composability
-- with type-changing lenses, they are the same in most cases
data DynamicRef s t = DynamicRef
  { dynamicRefRead    :: IO s
  , dynamicRefUpdates :: Event (Update s)
  , dynamicRefModify  :: (s -> t) -> IO ()
  }

type DynamicRef' s = DynamicRef s s

-- | Create new 'Dynamic' and a function to update the value
newDynamicRef :: a -> IO (DynamicRef a a)
newDynamicRef initial = do
  ref <- newIORef initial
  e <- newEventRef
  let
    modify = \f -> do
      old <- readIORef ref
      let new = f old
      writeIORef ref new
      triggerEvent e (Update old new)
  pure $ DynamicRef (readIORef ref) (getEvent e) modify

mapMaybeD :: b -> (Update a -> Maybe b) -> Dynamic a -> IO (Dynamic b)
mapMaybeD def f Dynamic{..} = do
  latestRef <- newIORef def
  let
    read = readIORef latestRef
    updates = flip mapMaybeE' dynamicUpdates \upd ->
      case f upd of
        Just new -> do
          old <- readIORef latestRef
          writeIORef latestRef new
          pure $ Just (Update old new)
        Nothing  -> pure Nothing
  pure (Dynamic read updates)

constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

fromDynamicRef :: DynamicRef s t -> Dynamic s
fromDynamicRef (DynamicRef read updates _) = Dynamic read updates

overDynamicRef :: Lens s t a b -> DynamicRef s t -> DynamicRef a b
overDynamicRef stab DynamicRef{..} = DynamicRef read updates modify where
  read    = fmap (getConst . stab Const) dynamicRefRead
  updates = fmap (fmap (getConst . stab Const)) dynamicRefUpdates
  modify  = \f -> dynamicRefModify (over stab f)

instance Functor Dynamic where
  fmap f Dynamic{..} =
    Dynamic (fmap f dynamicRead) (fmap (fmap f) dynamicUpdates)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) = error "Unimplemented"
