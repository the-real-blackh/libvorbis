{-# LANGUAGE ForeignFunctionInterface #-}

module Vorbisfile.Decoding where

import           Vorbisfile.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

foreign import ccall "ov_read"
  ov_read
    :: Ptr OggVorbisFile -- ^ vf
    -> Ptr #{type char}  -- ^ buffer
    -> #{type int}       -- ^ length
    -> #{type int}       -- ^ bigendianp
    -> #{type int}       -- ^ word
    -> #{type int}       -- ^ sgned
    -> Ptr #{type int}   -- ^ bitstream
    -> IO #type long



foreign import ccall "ov_read_float"
  ov_read_float
    :: Ptr OggVorbisFile             -- ^ vf
    -> Ptr (Ptr (Ptr #{type float})) -- ^ pcm_channels
    -> #{type int}                   -- ^ samples
    -> Ptr #{type int}               -- ^ bitstream
    -> IO #type long




type FilterFunc = Ptr (Ptr #{type float}) -- ^ pcm
               -> #{type long}            -- ^ channels
               -> #{type long}            -- ^ samples
               -> Ptr ()                  -- ^ filter_param
               -> IO (Ptr ())

foreign import ccall "ov_read_filter"
  ov_read_filter
    :: Ptr OggVorbisFile -- ^ vf
    -> Ptr #{type char}  -- ^ buffer
    -> #{type int}       -- ^ length
    -> #{type int}       -- ^ bigendianp
    -> #{type int}       -- ^ word
    -> #{type int}       -- ^ sgned
    -> Ptr #{type int}   -- ^ bitstream
    -> FunPtr FilterFunc -- ^ 
    -> Ptr ()            -- ^ filter_param
    -> IO #type long




foreign import ccall "ov_crosslap"
 ov_crosslap
   :: Ptr OggVorbisFile -- ^ old
   -> Ptr OggVorbisFile -- ^ new
   -> IO #type long
