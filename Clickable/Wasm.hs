{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Clickable.Wasm where

import Clickable.Internal
import Clickable.Types
import Control.Monad
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Map as Map
import Data.Tuple (swap)
import Data.Map (Map)
import Foreign.C.String (CStringLen)

foreign import ccall safe
  "clickable_eval_buffer" clickable_eval_buffer :: Ptr Word8 -> Int -> IO ()

env :: InternalEnv
{-# NOINLINE env #-}

continuations :: IORef (Map Word32 (IO Value -> IO ()))
{-# NOINLINE continuations #-}

buf :: CStringLen
{-# NOINLINE buf #-}

(env, continuations, buf) = unsafePerformIO $
  newInternalEnv (100 * 1024) \(ptr, len) ->
    clickable_eval_buffer (castPtr ptr) len

mkWasmApp :: ClickM () -> Ptr Word8 -> IO (Ptr Word8)
mkWasmApp app p | p == nullPtr = do
  runTransition env app
  return $ castPtr $ fst buf
mkWasmApp app inmsg = do
  msg <- loadMessage inmsg $ snd buf
  case msg of
    Just (StartMsg _flags) ->
      runTransition env app
    Just (EventMsg eventId pload) ->
      runTransition env $
        triggerEvent (unsafeFromEventId eventId) pload
    Just (ResumeMsg contId pload) -> do
      awatingThread <- atomicModifyIORef' continuations $
        swap . Map.alterF (,Nothing) contId
      forM_ awatingThread \cont -> cont $ pure $ exprToValue pload
    _ -> error "mkWasmApp: Failed to parse incomming command"
  return $ castPtr $ fst buf

loadMessage :: Binary msg => Ptr a -> Int -> IO (Maybe msg)
loadMessage p len
  | nullPtr /= p = fmap (Just . Binary.decode . BSL.fromStrict) loadByteString
  | otherwise = return Nothing
  where
    loadByteString :: IO ByteString
    loadByteString = BSU.unsafePackCStringLen (castPtr p, len)
