{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Both.Internal where

import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import ccall unsafe "vorbis_block_clear"
  vorbis_block_clear' :: Ptr VorbisBlock -> IO #type int



foreign import ccall unsafe "vorbis_block_init"
  vorbis_block_init' :: Ptr VorbisDspState -> Ptr VorbisBlock -> IO #type int



foreign import ccall unsafe "vorbis_granule_time"
  vorbis_granule_time' :: Ptr VorbisDspState -> Ogg_Int64_t -> IO #type double



foreign import ccall unsafe "vorbis_info_blocksize"
  vorbis_info_blocksize' :: Ptr VorbisInfo -> #{type int} -> IO #type int



foreign import ccall unsafe "vorbis_version_string"
  vorbis_version_string' :: Ptr #type char
