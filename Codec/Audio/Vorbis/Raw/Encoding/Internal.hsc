{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Encoding.Internal where

import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import ccall unsafe "vorbis_analysis"
  vorbis_analysis' :: Ptr VorbisBlock -> Ptr OggPacket -> IO #type int



foreign import ccall unsafe "vorbis_analysis_blockout"
  vorbis_analysis_blockout' :: Ptr VorbisDspState -> Ptr VorbisBlock -> IO #type int



foreign import ccall unsafe "vorbis_analysis_headerout"
  vorbis_analysis_headerout'
    :: Ptr VorbisDspState
    -> Ptr VorbisComment
    -> Ptr OggPacket
    -> Ptr OggPacket
    -> Ptr OggPacket
    -> IO #type int



foreign import ccall unsafe "vorbis_analysis_init"
  vorbis_analysis_init' :: Ptr VorbisDspState -> Ptr VorbisInfo -> IO #type int



foreign import ccall unsafe "vorbis_analysis_wrote"
  vorbis_analysis_wrote' :: Ptr VorbisDspState -> #{type int} -> IO #type int



foreign import ccall unsafe "vorbis_bitrate_addblock"
  vorbis_bitrate_addblock' :: Ptr VorbisBlock -> IO #type int



foreign import ccall unsafe "vorbis_bitrate_flushpacket"
  vorbis_bitrate_flushpacket' :: Ptr VorbisBlock -> Ptr OggPacket -> IO #type int
