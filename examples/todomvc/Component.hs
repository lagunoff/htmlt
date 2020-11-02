module Component
  ( fix1
  , compose1
  , Component
  , module X
  ) where

import Massaraksh as X
import Control.Natural as X (type (~>))

type Component w = (w ~> Html) -> (w ~> Html)

fix1 :: (w ~> m -> w ~> m) -> w ~> m
fix1 f = f (fix1 f)

compose1
  :: (w ~> m -> w ~> m)
  -> (w ~> m -> w ~> m)
  -> w ~> m -> w ~> m
compose1 a b wm = a (b wm)
{-# INLINE compose1 #-}
