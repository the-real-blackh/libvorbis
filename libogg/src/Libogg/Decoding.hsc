{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libogg.Decoding where

import           Libogg.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import CALLCV "ogg/ogg.h ogg_sync_init"
  ogg_sync_init
    :: Ptr Ogg_sync_state -- ^ oy
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_sync_check"
  ogg_sync_check
    :: Ptr Ogg_sync_state -- ^ oy
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_sync_clear"
  ogg_sync_clear
    :: Ptr Ogg_sync_state -- ^ oy
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_sync_destroy"
  ogg_sync_destroy
    :: Ptr Ogg_sync_state -- ^ oy
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_sync_reset"
  ogg_sync_reset
    :: Ptr Ogg_sync_state -- ^ oy
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_sync_buffer"
  ogg_sync_buffer
    :: Ptr Ogg_sync_state      -- ^ oy
    -> #{type long}          -- ^ size
    -> IO (Ptr #{type char})



foreign import CALLCV "ogg/ogg.h ogg_sync_wrote"
  ogg_sync_wrote
    :: Ptr Ogg_sync_state -- ^ oy
    -> #{type long}
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_sync_pageseek"
  ogg_sync_pageseek
    :: Ptr Ogg_sync_state -- ^ oy
    -> Ptr Ogg_page      -- ^ bytes
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_sync_pageout"
  ogg_sync_pageout
    :: Ptr Ogg_sync_state -- ^ oy
    -> Ptr Ogg_page      -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_pagein"
  ogg_stream_pagein
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_page        -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_packetout"
  ogg_stream_packetout
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_packet      -- ^ op
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_packetpeek"
  ogg_stream_packetpeek
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_packet      -- ^ op
    -> IO #type int
