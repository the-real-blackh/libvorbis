{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libvorbis.Both where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import CALLCV "vorbis/codec.h vorbis_block_clear"
  vorbis_block_clear
    :: Ptr Vorbis_block -- ^ vb
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_block_init"
  vorbis_block_init
    :: Ptr Vorbis_dsp_state -- ^ v
    -> Ptr Vorbis_block    -- ^ vb
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_dsp_clear"
  vorbis_dsp_clear
    :: Ptr Vorbis_dsp_state -- ^ v
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_granule_time"
  vorbis_granule_time
    :: Ptr Vorbis_dsp_state -- ^ v
    -> Ogg_int64_t        -- ^ granulepos
    -> IO #type double



foreign import CALLCV "vorbis/codec.h vorbis_info_blocksize"
  vorbis_info_blocksize
    :: Ptr Vorbis_info -- ^ vi
    -> #{type int}    -- ^ zo
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_info_clear"
  vorbis_info_clear
    :: Ptr Vorbis_info -- ^ vi
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_info_init"
  vorbis_info_init
    :: Ptr Vorbis_info -- ^ vi
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_version_string"
  vorbis_version_string
    :: IO (Ptr #type char)
