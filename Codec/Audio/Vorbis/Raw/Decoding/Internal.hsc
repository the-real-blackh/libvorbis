{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Decoding.Internal where

import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import ccall unsafe "vorbis_packet_blocksize"
  vorbis_packet_blocksize' :: Ptr VorbisInfo -> Ptr OggPacket -> IO #type long



foreign import ccall unsafe "vorbis_synthesis"
  vorbis_synthesis' :: Ptr VorbisInfo -> Ptr OggPacket -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_blockin"
  vorbis_synthesis_blockin' :: Ptr VorbisDspState -> Ptr VorbisBlock -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_halfrate"
  vorbis_synthesis_halfrate' :: Ptr VorbisInfo -> #{type int} -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_halfrate_p"
  vorbis_synthesis_halfrate_p' :: Ptr VorbisInfo -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_headerin"
  vorbis_synthesis_headerin' :: Ptr VorbisInfo -> Ptr VorbisComment -> Ptr OggPacket -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_idheader"
  vorbis_synthesis_idheader' :: Ptr OggPacket -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_init"
  vorbis_synthesis_init' :: Ptr VorbisDspState -> Ptr VorbisInfo -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_lapout"
  vorbis_synthesis_lapout' :: Ptr VorbisDspState -> Ptr (Ptr (Ptr #{type float})) -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_pcmout"
  vorbis_synthesis_pcmout' :: Ptr VorbisDspState -> Ptr (Ptr (Ptr #{type float})) -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_read"
  vorbis_synthesis_read' :: Ptr VorbisDspState -> #{type int} -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_restart"
  vorbis_synthesis_restart' :: Ptr VorbisDspState -> IO #type int



foreign import ccall unsafe "vorbis_synthesis_trackonly"
  vorbis_synthesis_trackonly' :: Ptr VorbisBlock -> Ptr OggPacket -> IO #type int
