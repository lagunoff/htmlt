{-# LANGUAGE InstanceSigs #-}
module Massaraksh.Dynamic where

import Data.IORef
import Massaraksh.Event
import GHC.Generics

data Update a = Update { updOld :: a , updNew :: a }
  deriving (Show, Eq, Generic, Functor)

-- | FIXME: Name was adopted from @reflex@ library, originally this
-- type was named @Store@ this is how js frameworks call similar
-- entities. The problem is that this name conflicts with @Dynamic@
-- from base which is also used by this library
data Dynamic a = Dynamic
  { dynRead    :: IO a -- ^ Read current value
  , dynUpdates :: Event (Update a) -- ^ Event that fires when the value changes
  }

-- | Result of running 'newDynamic'. Parameters 's' and 't' are
-- usually the same. Two parameters are needed for better
-- composability with lenses
data DynamicRef s t = DynamicRef
  { drefValue  :: Dynamic s
  , drefModify :: (s -> t) -> IO () }

-- | Create new 'Dynamic' and a function to update the value
newDynamicRef :: a -> IO (DynamicRef a a)
newDynamicRef initial = do
  ref <- newIORef initial
  EventRef{..} <- newEventRef
  let
    drefValue = Dynamic (readIORef ref) erefValue
    drefModify f = do
      old <- readIORef ref
      let new = f old
      writeIORef ref new
      erefPush (Update old new)
  pure DynamicRef{..}

mapMaybeD :: b -> (Update a -> Maybe b) -> Dynamic a -> IO (Dynamic b)
mapMaybeD def f Dynamic{..} = do
  latestRef <- newIORef def
  let
    dynRead' = readIORef latestRef
    dynUpdates' = flip mapMaybeIOE dynUpdates \update ->
      case f update of
        Just new -> do
          old <- readIORef latestRef
          writeIORef latestRef new
          pure $ Just (Update old new)
        Nothing  -> pure Nothing
  pure (Dynamic dynRead' dynUpdates')

constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

instance Functor Dynamic where
  fmap f Dynamic{..} =
    Dynamic (fmap f dynRead) (fmap (fmap f) dynUpdates)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) = error "Unimplemented"