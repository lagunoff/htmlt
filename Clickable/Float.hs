{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-| Normal `Data.Binary.Binary` instances for floating-point numbers
replacing the impractical and unnatural encoding in the default
instances for `Float` and `Double`
-}
module Clickable.Float where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


newtype Float64 = Float64 {unFloat64 :: Double}
  deriving newtype (Show, Ord, Eq)

instance Binary Float64 where
  put = putDoublele . unFloat64
  get = fmap Float64 getDoublele


newtype Float32 = Float32 {unFloat32 :: Float}
  deriving newtype (Show, Ord, Eq)

instance Binary Float32 where
  put = putFloatle . unFloat32
  get = fmap Float32 getFloatle
