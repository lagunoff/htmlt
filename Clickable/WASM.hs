{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module Clickable.WASM where

import Clickable.Internal
import Clickable.Types
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.Word
import Foreign.C.String (CStringLen)
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.IORef
import Control.Monad
import Unsafe.Coerce
import System.IO
import Debug.Trace

foreign import ccall safe
  "clickable_eval_buffer" clickable_eval_buffer :: Ptr Word8 -> Int -> IO ()

env :: InternalEnv
{-# NOINLINE env #-}

buf :: CStringLen
{-# NOINLINE buf #-}

(env, buf) = unsafePerformIO $
  newInternalEnv (100 * 1024) \(ptr, len) ->
    clickable_eval_buffer (castPtr ptr) len

mkWasmApp :: (StartFlags -> JSM ()) -> Ptr Word8 -> IO (Ptr Word8)
mkWasmApp _app p | p == nullPtr = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  return $ castPtr $ fst buf
mkWasmApp app inmsg = do
  msg <- loadMessage inmsg $ snd buf
  case msg of
    Just (StartMsg flags) ->
      runJSM env $ app flags
    Just (EventMsg eventId pload) ->
      runJSM env $ triggerEvent (unsafeFromEventId eventId) pload
    Just (ResumeMsg contId pload) -> do
      cont <- atomicModifyIORef' env.ien_state $ lookupCont $ coerce contId
      forM_ cont \c -> runJSM env $ c.sub_callback $ unsafeCoerce $ ((pure pload) :: IO JSVal)
    Just BeforeUnloadMsg -> do
      runJSM env $ freeScope env.ien_scope
    _ -> error "mkWasmApp: Failed to parse incomming command"
  pure $ castPtr $ fst buf
  where
    lookupCont :: EventId -> InternalState -> (InternalState, [Subscription Any])
    lookupCont eventId s = (s {ist_subscriptions = subs}, fromMaybe [] cont) where
      (cont, subs) = Map.alterF (,Nothing) eventId $ s.ist_subscriptions

loadMessage :: Binary msg => Ptr a -> Int -> IO (Maybe msg)
loadMessage p len
  | nullPtr /= p = fmap (Just . Binary.decode . BSL.fromStrict) loadByteString
  | otherwise = return Nothing
  where
    loadByteString :: IO ByteString
    loadByteString = BSU.unsafePackCStringLen (castPtr p, len)

data ResourcesShow = ResourcesShow {
  rsr_parent :: ScopeId,
  rsr_linked :: [ScopeId],
  rsr_finalizers :: [String]
} deriving Show

instance Show Resources where
  show = show . convert
    where
      convert :: Resources -> ResourcesShow
      convert Resources{..} = ResourcesShow {
        rsr_parent,
        rsr_linked,
        rsr_finalizers = map (const "<reducted>") rsr_finalizers
      }
