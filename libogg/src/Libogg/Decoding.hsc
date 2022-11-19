{-# LANGUAGE ForeignFunctionInterface #-}

module Libogg.Decoding where

import           Libogg.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import ccall "ogg_sync_init"
  ogg_sync_init
    :: Ptr OggSyncState -- ^ oy
    -> IO #type int



foreign import ccall "ogg_sync_check"
  ogg_sync_check
    :: Ptr OggSyncState -- ^ oy
    -> IO #type int



foreign import ccall "ogg_sync_clear"
  ogg_sync_clear
    :: Ptr OggSyncState -- ^ oy
    -> IO #type int



foreign import ccall "ogg_sync_destroy"
  ogg_sync_destroy
    :: Ptr OggSyncState -- ^ oy
    -> IO #type int



foreign import ccall "ogg_sync_reset"
  ogg_sync_reset
    :: Ptr OggSyncState -- ^ oy
    -> IO #type int



foreign import ccall "ogg_sync_buffer"
  ogg_sync_buffer
    :: Ptr OggSyncState      -- ^ oy
    -> #{type long}          -- ^ size
    -> IO (Ptr #{type char})



foreign import ccall "ogg_sync_wrote"
  ogg_sync_wrote
    :: Ptr OggSyncState -- ^ oy
    -> #{type long}
    -> IO #type int



foreign import ccall "ogg_sync_pageseek"
  ogg_sync_pageseek
    :: Ptr OggSyncState -- ^ oy
    -> Ptr OggPage      -- ^ bytes
    -> IO #type int



foreign import ccall "ogg_sync_pageout"
  ogg_sync_pageout
    :: Ptr OggSyncState -- ^ oy
    -> Ptr OggPage      -- ^ og
    -> IO #type int



foreign import ccall "ogg_stream_pagein"
  ogg_stream_pagein
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPage        -- ^ og
    -> IO #type int



foreign import ccall "ogg_stream_packetout"
  ogg_stream_packetout
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPacket      -- ^ op
    -> IO #type int



foreign import ccall "ogg_stream_packetpeek"
  ogg_stream_packetpeek
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPacket      -- ^ op
    -> IO #type int
