{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libogg.Encoding where

import           Libogg.Types

import           Data.Int
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import CALLCV "ogg/ogg.h ogg_stream_packetin"
  ogg_stream_packetin
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_packet      -- ^ cp
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_pageout"
  ogg_stream_pageout
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_page        -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_pageout_fill"
  ogg_stream_pageout_fill
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_page        -- ^ og
    -> #{type int}        -- ^ fillbytes
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_flush"
  ogg_stream_flush
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_page        -- ^ og
    -> IO #type int



foreign import CALLCV "ogg/ogg.h ogg_stream_flush_fill"
  ogg_stream_flush_fill
    :: Ptr Ogg_stream_state -- ^ os
    -> Ptr Ogg_page        -- ^ og
    -> #{type int}        -- ^ fillbytes
    -> IO #type int
