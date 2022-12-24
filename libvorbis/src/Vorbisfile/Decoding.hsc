{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Vorbisfile.Decoding where

import           Vorbisfile.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

foreign import CALLCV "vorbis/vorbisfile.h ov_read"
  ov_read
    :: Ptr OggVorbis_File -- ^ vf
    -> Ptr #{type char}   -- ^ buffer
    -> #{type int}        -- ^ length
    -> #{type int}        -- ^ bigendianp
    -> #{type int}        -- ^ word
    -> #{type int}        -- ^ sgned
    -> Ptr #{type int}    -- ^ bitstream
    -> IO #type long



foreign import CALLCV "vorbis/vorbisfile.h ov_read_float"
  ov_read_float
    :: Ptr OggVorbis_File             -- ^ vf
    -> Ptr (Ptr (Ptr #{type float})) -- ^ pcm_channels
    -> #{type int}                   -- ^ samples
    -> Ptr #{type int}               -- ^ bitstream
    -> IO #type long




type Filter_func =
          Ptr (Ptr #{type float}) -- ^ pcm
       -> #{type long}            -- ^ channels
       -> #{type long}            -- ^ samples
       -> Ptr ()                  -- ^ filter_param
       -> IO (Ptr ())

foreign import CALLCV "vorbis/vorbisfile.h ov_read_filter"
  ov_read_filter
    :: Ptr OggVorbis_File -- ^ vf
    -> Ptr #{type char}   -- ^ buffer
    -> #{type int}        -- ^ length
    -> #{type int}        -- ^ bigendianp
    -> #{type int}        -- ^ word
    -> #{type int}        -- ^ sgned
    -> Ptr #{type int}    -- ^ bitstream
    -> FunPtr Filter_func
    -> Ptr ()             -- ^ filter_param
    -> IO #type long




foreign import CALLCV "vorbis/vorbisfile.h ov_crosslap"
 ov_crosslap
   :: Ptr OggVorbis_File -- ^ old
   -> Ptr OggVorbis_File -- ^ new
   -> IO #type long
