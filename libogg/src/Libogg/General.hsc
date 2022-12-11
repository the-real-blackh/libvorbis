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
    :: Ptr OggStreamState -- ^ os
    -> #{type int}        -- ^ serialno
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_check"
  ogg_stream_check
    :: Ptr OggStreamState -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_clear"
  ogg_stream_clear
    :: Ptr OggStreamState -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_reset"
  ogg_stream_reset
    :: Ptr OggStreamState -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_reset_serialno"
  ogg_stream_reset_serialno
    :: Ptr OggStreamState -- ^ os
    -> #{type int}        -- ^ serialno
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_destroy"
  ogg_stream_destroy
    :: Ptr OggStreamState -- ^ os
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_version"
  ogg_page_version
    :: Ptr OggPage  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_continued"
  ogg_page_continued
    :: Ptr OggPage  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_packets"
  ogg_page_packets
    :: Ptr OggPage  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_bos"
  ogg_page_bos
    :: Ptr OggPage  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_eos"
  ogg_page_eos
    :: Ptr OggPage  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_granulepos"
  ogg_page_granulepos
    :: Ptr OggPage    -- ^ og
    -> IO Ogg_Int64_t



foreign import CALLCV "ogg/ogg.h ogg_page_serialno"
  ogg_page_serialno
    :: Ptr OggPage  -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_page_pageno"
  ogg_page_pageno
    :: Ptr OggPage   -- ^ og
    -> IO #type long



foreign import CALLCV "ogg/ogg.h ogg_packet_clear"
  ogg_packet_clear
    :: Ptr OggPacket -- ^ op
    -> IO ()



foreign import CALLCV "ogg/ogg.h ogg_page_checksum_set"
  ogg_page_checksum_set
    :: Ptr OggPage -- ^ og
    -> IO ()
