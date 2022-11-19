{-# LANGUAGE ForeignFunctionInterface #-}

module Libogg.Encoding where

import           Libogg.Types

import           Data.Int
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import ccall "ogg_stream_packetin"
  ogg_stream_packetin
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPacket      -- ^ cp
    -> IO #type int



foreign import ccall "ogg_stream_pageout"
  ogg_stream_pageout
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPacket      -- ^ og
    -> IO #type int



foreign import ccall "ogg_stream_pageout_fill"
  ogg_stream_pageout_fill
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPacket      -- ^ og
    -> #{type int}        -- ^ fillbytes
    -> IO #type int



foreign import ccall "ogg_stream_flush"
  ogg_stream_flush
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPacket      -- ^ og
    -> IO #type int



foreign import ccall "ogg_stream_flush_fill"
  ogg_stream_flush_fill
    :: Ptr OggStreamState -- ^ os
    -> Ptr OggPacket      -- ^ og
    -> #{type int}        -- ^ fillbytes
    -> IO #type int
