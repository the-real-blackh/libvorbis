{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Vorbisfile.File where

import           Libogg
import           Libvorbis.Types
import           Vorbisfile.Types


import           Data.Int
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

foreign import CALLCV "vorbis/vorbisfile.h ov_bitrate"
  ov_bitrate
    :: Ptr OggVorbis_File -- ^ vf
    -> #{type int}       -- ^ i
    -> IO #type long



foreign import CALLCV "vorbis/vorbisfile.h ov_bitrate_instant"
  ov_bitrate_instant
    :: Ptr OggVorbis_File -- ^ vf
    -> IO #type long



foreign import CALLCV "vorbis/vorbisfile.h ov_streams"
  ov_streams
    :: Ptr OggVorbis_File -- ^ vf
    -> IO #type long



foreign import CALLCV "vorbis/vorbisfile.h ov_seekable"
  ov_seekable
    :: Ptr OggVorbis_File -- ^ vf
    -> IO #type long



foreign import CALLCV "vorbis/vorbisfile.h ov_serialnumber"
  ov_serialnumber
    :: Ptr OggVorbis_File -- ^ vf
    -> #{type int}       -- ^ i
    -> IO #type long



foreign import CALLCV "vorbis/vorbisfile.h ov_raw_total"
  ov_raw_total
    :: Ptr OggVorbis_File -- ^ vf
    -> #{type int}       -- ^ i
    -> IO Ogg_int64_t



foreign import CALLCV "vorbis/vorbisfile.h ov_pcm_total"
  ov_pcm_total
    :: Ptr OggVorbis_File -- ^ vf
    -> #{type int}       -- ^ i
    -> IO Ogg_int64_t



foreign import CALLCV "vorbis/vorbisfile.h ov_time_total"
  ov_time_total
    :: Ptr OggVorbis_File -- ^ vf
    -> #{type int}       -- ^ i
    -> IO #type double



foreign import CALLCV "vorbis/vorbisfile.h ov_raw_tell"
  ov_raw_tell
    :: Ptr OggVorbis_File -- ^ vf
    -> IO Ogg_int64_t



foreign import CALLCV "vorbis/vorbisfile.h ov_pcm_tell"
  ov_pcm_tell
    :: Ptr OggVorbis_File -- ^ vf
    -> IO Ogg_int64_t



foreign import CALLCV "vorbis/vorbisfile.h ov_time_tell"
  ov_time_tell
    :: Ptr OggVorbis_File -- ^ vf
    -> IO #type double



foreign import CALLCV "vorbis/vorbisfile.h ov_info"
  ov_info
    :: Ptr OggVorbis_File -- ^ vf
    -> #{type int}       -- ^ link
    -> IO (Ptr Vorbis_info)



foreign import CALLCV "vorbis/vorbisfile.h ov_comment"
  ov_comment
    :: Ptr OggVorbis_File      -- ^ vf
    -> #{type int}            -- ^ link
    -> IO (Ptr Vorbis_comment)
