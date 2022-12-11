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
    :: Ptr VorbisBlock -- ^ vb
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_block_init"
  vorbis_block_init
    :: Ptr VorbisDspState -- ^ v
    -> Ptr VorbisBlock    -- ^ vb
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_dsp_clear"
  vorbis_dsp_clear
    :: Ptr VorbisDspState -- ^ v
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_granule_time"
  vorbis_granule_time
    :: Ptr VorbisDspState -- ^ v
    -> Ogg_Int64_t        -- ^ granulepos
    -> IO #type double



foreign import CALLCV "vorbis/codec.h vorbis_info_blocksize"
  vorbis_info_blocksize
    :: Ptr VorbisInfo -- ^ vi
    -> #{type int}    -- ^ zo
    -> IO #type int



foreign import CALLCV "vorbis/codec.h vorbis_info_clear"
  vorbis_info_clear
    :: Ptr VorbisInfo -- ^ vi
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_info_init"
  vorbis_info_init
    :: Ptr VorbisInfo -- ^ vi
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_version_string"
  vorbis_version_string
    :: IO (Ptr #type char)
