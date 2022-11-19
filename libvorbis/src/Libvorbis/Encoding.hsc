{-# LANGUAGE ForeignFunctionInterface #-}

module Libvorbis.Encoding where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import ccall "vorbis_analysis"
  vorbis_analysis
    :: Ptr VorbisBlock -- ^ vb
    -> Ptr OggPacket   -- ^ op
    -> IO #type int



foreign import ccall "vorbis_analysis_blockout"
  vorbis_analysis_blockout
    :: Ptr VorbisDspState -- ^ v
    -> Ptr VorbisBlock    -- ^ vb
    -> IO #type int



foreign import ccall "vorbis_analysis_buffer"
  vorbis_analysis_buffer
    :: Ptr VorbisDspState           -- ^ v
    -> #{type int}                  -- ^ vals
    -> IO (Ptr (Ptr #{type float}))



foreign import ccall "vorbis_analysis_headerout"
  vorbis_analysis_headerout
    :: Ptr VorbisDspState -- ^ v
    -> Ptr VorbisComment  -- ^ vc
    -> Ptr OggPacket      -- ^ op
    -> Ptr OggPacket      -- ^ op_comm
    -> Ptr OggPacket      -- ^ op_code
    -> IO #type int



foreign import ccall "vorbis_analysis_init"
  vorbis_analysis_init
    :: Ptr VorbisDspState -- ^ v
    -> Ptr VorbisInfo     -- ^ vi
    -> IO #type int



foreign import ccall "vorbis_analysis_wrote"
  vorbis_analysis_wrote
    :: Ptr VorbisDspState -- ^ v
    -> #{type int}        -- ^ vals
    -> IO #type int



foreign import ccall "vorbis_bitrate_addblock"
  vorbis_bitrate_addblock
    :: Ptr VorbisBlock -- ^ vb
    -> IO #type int



foreign import ccall "vorbis_bitrate_flushpacket"
  vorbis_bitrate_flushpacket
    :: Ptr VorbisBlock -- ^ vd
    -> Ptr OggPacket   -- ^ op
    -> IO #type int
