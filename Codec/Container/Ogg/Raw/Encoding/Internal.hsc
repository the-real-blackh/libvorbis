{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.Encoding.Internal where

import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import ccall unsafe "ogg_stream_packetin"
  ogg_stream_packetin' :: Ptr OggStreamState -> Ptr OggPacket -> IO #type int



foreign import ccall unsafe "ogg_stream_pageout"
  ogg_stream_pageout' :: Ptr OggStreamState -> Ptr OggPacket -> IO #type int



foreign import ccall unsafe "ogg_stream_pageout_fill"
  ogg_stream_pageout_fill' :: Ptr OggStreamState -> Ptr OggPacket -> #{type int} -> IO #type int



foreign import ccall unsafe "ogg_stream_flush"
  ogg_stream_flush' :: Ptr OggStreamState -> Ptr OggPacket -> IO #type int



foreign import ccall unsafe "ogg_stream_flush_fill"
  ogg_stream_flush_fill' :: Ptr OggStreamState -> Ptr OggPacket -> #{type int} -> IO #type int
