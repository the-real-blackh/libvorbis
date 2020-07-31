{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.Decoding.Internal where

import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import ccall unsafe "ogg_sync_init"
  ogg_sync_init' :: Ptr OggSyncState -> IO #type int



foreign import ccall unsafe "ogg_sync_check"
  ogg_sync_check' :: Ptr OggSyncState -> IO #type int



foreign import ccall unsafe "ogg_sync_clear"
  ogg_sync_clear' :: Ptr OggSyncState -> IO #type int



foreign import ccall unsafe "ogg_sync_destroy"
  ogg_sync_destroy' :: Ptr OggSyncState -> IO #type int



foreign import ccall unsafe "ogg_sync_reset"
  ogg_sync_reset' :: Ptr OggSyncState -> IO #type int



foreign import ccall unsafe "ogg_sync_buffer"
  ogg_sync_buffer' :: Ptr OggSyncState -> #{type long} -> IO (Ptr #{type char})



foreign import ccall unsafe "ogg_sync_wrote"
  ogg_sync_wrote' :: Ptr OggSyncState -> #{type long} -> IO #type int



foreign import ccall unsafe "ogg_sync_pageseek"
  ogg_sync_pageseek' :: Ptr OggSyncState -> Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_sync_pageout"
  ogg_sync_pageout' :: Ptr OggSyncState -> Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_stream_pagein"
  ogg_stream_pagein' :: Ptr OggSyncState -> Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_stream_packetout"
  ogg_stream_packetout' :: Ptr OggStreamState -> Ptr OggPacket -> IO #type int



foreign import ccall unsafe "ogg_stream_packetpeek"
  ogg_stream_packetpeek' :: Ptr OggStreamState -> Ptr OggPacket -> IO #type int
