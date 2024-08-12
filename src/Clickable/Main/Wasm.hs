module Clickable.Main.Wasm where

import Control.Monad.State
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Clickable.FFI qualified as FFI
import Clickable.Protocol
import Clickable.Protocol.Value
import Clickable.Core
import Clickable.Internal qualified as Internal
import Clickable.Types

runWasm :: (StartFlags -> ClickM ()) -> Ptr Word8 -> IO ()
runWasm app p = mdo
  jmsg <- loadMessage p
  case jmsg of
    Just (Start flags) ->
      launchClickM internalEnv $ app flags
    Just (TriggerCallbackMsg arg eid) ->
      launchClickM internalEnv $ modify $ Internal.triggerEvent (unsafeFromEventId eid) arg
    Just BeforeUnload ->
      launchClickM internalEnv $ freeScope True $ ResourceScope $ Int32Le Internal.emptyState.next_id
    _ ->
      return ()

internalEnv :: InternalEnv
internalEnv = unsafePerformIO $ Internal.newInternalEnv sendMessage

sendMessage :: Expr -> IO Value
sendMessage expr = do
  hptr <- storeByteString $ BSL.toStrict $ Binary.encode $ EvalExpr (Int32Le 0) expr
  jptr <- FFI.js_evalMessage hptr
  Alloc.free hptr
  jmsg <- loadMessage jptr
  case jmsg of
    Just (Return _ v) -> return v
    _ -> return Vnull
  where
    storeByteString :: ByteString -> IO (Ptr a)
    storeByteString bs = do
      let len = BS.length bs
      dest <- Alloc.callocBytes (len + 8)
      poke @Word64 dest (fromIntegral len)
      BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
        copyBytes (dest `plusPtr` 8) src len
      return (castPtr dest)

loadMessage :: Binary msg => Ptr a -> IO (Maybe msg)
loadMessage p
  | nullPtr /= p = fmap (Just . Binary.decode . BSL.fromStrict) . loadByteString $ p
  | otherwise = return Nothing
  where
    loadByteString :: Ptr a -> IO ByteString
    loadByteString ptr = do
      len <- peek @Word64 (castPtr ptr)
      let contentPtr = ptr `plusPtr` 8
      BSU.unsafePackCStringFinalizer contentPtr (fromIntegral len) (Alloc.free ptr)
