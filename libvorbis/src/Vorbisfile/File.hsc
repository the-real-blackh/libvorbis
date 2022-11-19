{-# LANGUAGE ForeignFunctionInterface #-}

module Vorbisfile.File where

import           Libogg
import           Libvorbis.Types
import           Vorbisfile.Types


import           Data.Int
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

foreign import ccall "ov_bitrate"
  ov_bitrate
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type int}       -- ^ i
    -> IO #type long



foreign import ccall "ov_bitrate_instant"
  ov_bitrate_instant
    :: Ptr OggVorbisFile -- ^ vf
    -> IO #type long



foreign import ccall "ov_streams"
  ov_streams
    :: Ptr OggVorbisFile -- ^ vf
    -> IO #type long



foreign import ccall "ov_seekable"
  ov_seekable
    :: Ptr OggVorbisFile -- ^ vf
    -> IO #type long



foreign import ccall "ov_serialnumber"
  ov_serialnumber
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type int}       -- ^ i
    -> IO #type long



foreign import ccall "ov_raw_total"
  ov_raw_total
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type int}       -- ^ i
    -> IO Ogg_Int64_t



foreign import ccall "ov_pcm_total"
  ov_pcm_total
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type int}       -- ^ i
    -> IO Ogg_Int64_t



foreign import ccall "ov_time_total"
  ov_time_total
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type int}       -- ^ i
    -> IO #type double



foreign import ccall "ov_raw_tell"
  ov_raw_tell
    :: Ptr OggVorbisFile -- ^ vf
    -> IO Ogg_Int64_t



foreign import ccall "ov_pcm_tell"
  ov_pcm_tell
    :: Ptr OggVorbisFile -- ^ vf
    -> IO Ogg_Int64_t



foreign import ccall "ov_time_tell"
  ov_time_tell
    :: Ptr OggVorbisFile -- ^ vf
    -> IO #type double



foreign import ccall "ov_info"
  ov_info
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type int}       -- ^ link
    -> IO (Ptr VorbisInfo)



foreign import ccall "ov_comment"
  ov_comment
    :: Ptr OggVorbisFile      -- ^ vf
    -> #{type int}            -- ^ link
    -> IO (Ptr VorbisComment)