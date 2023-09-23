{-|
Implement the missing functionality, which is likely to be included in
the standard library at some point in the future.
-}
{-# LANGUAGE CPP #-}
#if defined(javascript_HOST_ARCH)
module JavaScript.Compat.String
  ( module JavaScript.Compat.String.JavaScript
  ) where
import JavaScript.Compat.String.JavaScript
#else
module JavaScript.Compat.String
  ( module JavaScript.Compat.String.Native
  ) where
import JavaScript.Compat.String.Native
#endif
