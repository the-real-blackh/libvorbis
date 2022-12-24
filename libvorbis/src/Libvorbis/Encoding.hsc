{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libvorbis.Encoding where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import CALLCV "vorbis/codec.h vorbis_analysis"
  vorbis_analysis
    :: Ptr Vorbis_block -- ^ vb
    -> Ptr Ogg_packet   -- ^ op
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_analysis_blockout"
  vorbis_analysis_blockout
    :: Ptr Vorbis_dsp_state -- ^ v
    -> Ptr Vorbis_block    -- ^ vb
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_analysis_buffer"
  vorbis_analysis_buffer
    :: Ptr Vorbis_dsp_state           -- ^ v
    -> #{type int}                  -- ^ vals
    -> IO (Ptr (Ptr #{type float}))



foreign import CALLCV "vorbis/codec.h vorbis_analysis_headerout"
  vorbis_analysis_headerout
    :: Ptr Vorbis_dsp_state -- ^ v
    -> Ptr Vorbis_comment  -- ^ vc
    -> Ptr Ogg_packet      -- ^ op
    -> Ptr Ogg_packet      -- ^ op_comm
    -> Ptr Ogg_packet      -- ^ op_code
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_analysis_init"
  vorbis_analysis_init
    :: Ptr Vorbis_dsp_state -- ^ v
    -> Ptr Vorbis_info     -- ^ vi
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_analysis_wrote"
  vorbis_analysis_wrote
    :: Ptr Vorbis_dsp_state -- ^ v
    -> #{type int}        -- ^ vals
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_bitrate_addblock"
  vorbis_bitrate_addblock
    :: Ptr Vorbis_block -- ^ vb
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_bitrate_flushpacket"
  vorbis_bitrate_flushpacket
    :: Ptr Vorbis_dsp_state -- ^ vd
    -> Ptr Ogg_packet      -- ^ op
    -> IO #type int
