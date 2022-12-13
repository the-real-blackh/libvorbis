{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libogg.General where

import           Libogg.Types

import           Data.Int
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import CALLCV "ogg/ogg.h ogg_stream_init"
  ogg_stream_init
    :: Ptr Ogg_stream_state -- ^ os
    -> #{type int}        -- ^ serialno
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_check"
  ogg_stream_check
    :: Ptr Ogg_stream_state -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_clear"
  ogg_stream_clear
    :: Ptr Ogg_stream_state -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_reset"
  ogg_stream_reset
    :: Ptr Ogg_stream_state -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_reset_serialno"
  ogg_stream_reset_serialno
    :: Ptr Ogg_stream_state -- ^ os
    -> #{type int}        -- ^ serialno
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_destroy"
  ogg_stream_destroy
    :: Ptr Ogg_stream_state -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_version"
  ogg_page_version
    :: Ptr Ogg_page  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_continued"
  ogg_page_continued
    :: Ptr Ogg_page  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_packets"
  ogg_page_packets
    :: Ptr Ogg_page  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_bos"
  ogg_page_bos
    :: Ptr Ogg_page  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_eos"
  ogg_page_eos
    :: Ptr Ogg_page  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_granulepos"
  ogg_page_granulepos
    :: Ptr Ogg_page    -- ^ og
    -> IO Ogg_int64_t



foreign import CALLCV "ogg/ogg.h ogg_page_serialno"
  ogg_page_serialno
    :: Ptr Ogg_page  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_pageno"
  ogg_page_pageno
    :: Ptr Ogg_page   -- ^ og
    -> IO #type long



foreign import CALLCV "ogg/ogg.h ogg_packet_clear"
  ogg_packet_clear
    :: Ptr Ogg_packet -- ^ op
    -> IO ()



foreign import CALLCV "ogg/ogg.h ogg_page_checksum_set"
  ogg_page_checksum_set
    :: Ptr Ogg_page -- ^ og
    -> IO ()
