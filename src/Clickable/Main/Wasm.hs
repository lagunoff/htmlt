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
import Clickable.Core
import Clickable.Internal qualified as Internal
import Clickable.Types

runWasm :: (StartFlags -> ClickM ()) -> Ptr Word8 -> IO ()
runWasm app p = mdo
  jmsg <- loadMessage p
  case jmsg of
    Start flags ->
      launchClickM internalEnv $ app flags <* syncPoint
    Return _value -> return ()
    TriggerCallbackMsg arg sourceId ->
      launchClickM internalEnv $ modify $ Internal.unsafeTrigger sourceId arg
    BeforeUnload ->
      launchClickM internalEnv $ freeScope True $ ResourceScope Internal.emptyState.next_id

internalEnv :: InternalEnv
internalEnv = unsafePerformIO $ Internal.newInternalEnv sendMessage

sendMessage :: HaskellMessage -> IO JavaScriptMessage
sendMessage hmsg = do
  hptr <- storeByteString $ BSL.toStrict $ Binary.encode hmsg
  jptr <- FFI.js_evalMessage hptr
  Alloc.free hptr
  loadMessage jptr
  where
    storeByteString :: ByteString -> IO (Ptr a)
    storeByteString bs = do
      let len = BS.length bs
      dest <- Alloc.callocBytes (len + 8)
      poke @Word64 dest (fromIntegral len)
      BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
        copyBytes (dest `plusPtr` 8) src len
      return (castPtr dest)

loadMessage :: Binary msg => Ptr a -> IO msg
loadMessage = fmap (Binary.decode . BSL.fromStrict) . loadByteString
  where
    loadByteString :: Ptr a -> IO ByteString
    loadByteString ptr = do
      len <- peek @Word64 (castPtr ptr)
      let contentPtr = ptr `plusPtr` 8
      BSU.unsafePackCStringFinalizer contentPtr (fromIntegral len) (Alloc.free ptr)
