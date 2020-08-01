{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.Decoding where

import           Codec.Container.Ogg.Raw.Decoding.Internal
import           Codec.Container.Ogg.Raw.Internal.Exception
import           Codec.Container.Ogg.Raw.Types

import           Control.Exception
import           Control.Monad (void)
import           Data.Int
import           Foreign.Marshal.Alloc
import           Foreign.Ptr

#include "ogg/ogg.h"

-- | Result is always zero and thus is dropped.
ogg_sync_init :: Ptr OggSyncState -> IO ()
ogg_sync_init = void . ogg_sync_init'




-- | Wrapper over 'ogg_sync_init' and 'ogg_sync_clear'. The passed in
--   'OggSyncState' pointer should not be used after this function terminates.
ogg_sync_with :: (Ptr OggSyncState -> IO a) -> IO a
ogg_sync_with action =
  alloca $ \vf ->
    bracket_ (ogg_sync_init vf)
             (ogg_sync_clear vf)
             (action vf)



-- | Throws 'OggError' on non-zero result.
ogg_sync_check :: Ptr OggSyncState -> IO ()
ogg_sync_check =
  oggError "ogg_sync_check" . ogg_sync_check'



-- | Result is always zero and thus is dropped.
ogg_sync_clear :: Ptr OggSyncState -> IO ()
ogg_sync_clear = void . ogg_sync_clear'



-- | Result is always zero and thus is dropped.
ogg_sync_destroy :: Ptr OggSyncState -> IO ()
ogg_sync_destroy = void . ogg_sync_destroy'



-- | Result is always zero and thus is dropped.
ogg_sync_reset :: Ptr OggSyncState -> IO ()
ogg_sync_reset = void . ogg_sync_reset'



-- | Returns 'Nothing' instead of 'nullPtr'.
ogg_sync_buffer :: Ptr OggSyncState -> #{type long} -> IO (Maybe (Ptr #{type char}))
ogg_sync_buffer oy size = do
  res <- ogg_sync_buffer' oy size
  return $ if res == nullPtr
             then Nothing
             else Just res



-- | Throws 'OggError' on non-zero result.
ogg_sync_wrote :: Ptr OggSyncState -> #{type long} -> IO ()
ogg_sync_wrote oy =
  oggError "ogg_sync_wrote" . ogg_sync_wrote' oy



-- | Helper data structure for a couple of functions with similar results.
--   Ones that return @n\/0\/-n@ are parametrized over @int@,
--   ones that return @1\/0\/-1@ are parametrized over '()'.
data PageSync n = Skipped n -- ^ trying to sync
                | Unsynced  -- ^ needs more data \/ internal error occured
                | Synced n  -- ^ is in sync



ogg_sync_pageseek :: Ptr OggSyncState -> Ptr OggPage -> IO (PageSync #{type int})
ogg_sync_pageseek oy og = do
  res <- ogg_sync_pageseek' oy og
  return $ case () of
             () | res > 0   -> Synced res
                | res < 0   -> Skipped res
                | otherwise -> Unsynced



ogg_sync_pageout :: Ptr OggSyncState -> Ptr OggPage -> IO (PageSync ())
ogg_sync_pageout oy og = do
  res <- ogg_sync_pageout' oy og
  case res of
    1  -> return $ Synced ()
    -1 -> return $ Skipped ()
    0  -> return $ Unsynced
    _  -> oggError' "ogg_sync_pageout" res -- Unreachable



-- | Throws 'OggError' on non-zero result.
ogg_stream_pagein :: Ptr OggSyncState -> Ptr OggPage -> IO ()
ogg_stream_pagein oy =
  oggError "ogg_stream_pagein" . ogg_stream_pagein' oy



ogg_stream_packetout :: Ptr OggStreamState -> Ptr OggPacket -> IO (PageSync ())
ogg_stream_packetout os op = do
  res <- ogg_stream_packetout' os op
  case res of
    1  -> return $ Synced ()
    -1 -> return $ Skipped ()
    0  -> return $ Unsynced
    _  -> oggError' "ogg_stream_packetout" res -- Unreachable



ogg_stream_packetpeek :: Ptr OggStreamState -> Ptr OggPacket -> IO (PageSync ())
ogg_stream_packetpeek os op = do
  res <- ogg_stream_packetpeek' os op
  case res of
    1  -> return $ Synced ()
    -1 -> return $ Skipped ()
    0  -> return $ Unsynced
    _  -> oggError' "ogg_stream_packetpeek" res -- Unreachable
