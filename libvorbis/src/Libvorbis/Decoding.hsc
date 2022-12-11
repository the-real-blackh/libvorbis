{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libvorbis.Decoding where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import CALLCV "vorbis/codec.h vorbis_packet_blocksize"
  vorbis_packet_blocksize
    :: Ptr VorbisInfo -- ^ vi
    -> Ptr OggPacket  -- ^ op
    -> IO #type long



foreign import CALLCV "vorbis/codec.h vorbis_synthesis"
  vorbis_synthesis
    :: Ptr VorbisBlock -- ^ vb
    -> Ptr OggPacket   -- ^ op
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_blockin"
  vorbis_synthesis_blockin
    :: Ptr VorbisDspState -- ^ v
    -> Ptr VorbisBlock    -- ^ vb
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_halfrate"
  vorbis_synthesis_halfrate
    :: Ptr VorbisInfo -- ^ v
    -> #{type int}    -- ^ flag
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_halfrate_p"
  vorbis_synthesis_halfrate_p
    :: Ptr VorbisInfo -- ^ v
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_headerin"
  vorbis_synthesis_headerin
    :: Ptr VorbisInfo    -- ^ vi
    -> Ptr VorbisComment -- ^ vc
    -> Ptr OggPacket     -- ^ op
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_idheader"
  vorbis_synthesis_idheader
    :: Ptr OggPacket -- ^ op
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_init"
  vorbis_synthesis_init
    :: Ptr VorbisDspState -- ^ v
    -> Ptr VorbisInfo     -- ^ vi
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_lapout"
  vorbis_synthesis_lapout
    :: Ptr VorbisDspState            -- ^ v
    -> Ptr (Ptr (Ptr #{type float})) -- ^ pcm
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_pcmout"
  vorbis_synthesis_pcmout
    :: Ptr VorbisDspState            -- ^ v
    -> Ptr (Ptr (Ptr #{type float})) -- ^ pcm
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_read"
  vorbis_synthesis_read
    :: Ptr VorbisDspState -- ^ v
    -> #{type int}        -- ^ samples
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_restart"
  vorbis_synthesis_restart
    :: Ptr VorbisDspState -- ^ v
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_synthesis_trackonly"
  vorbis_synthesis_trackonly
    :: Ptr VorbisBlock -- ^ vb
    -> Ptr OggPacket   -- ^ op
    -> IO #type int
