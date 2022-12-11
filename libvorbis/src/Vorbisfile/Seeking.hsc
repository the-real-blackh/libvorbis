{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Vorbisfile.Seeking where

import           Libogg
import           Vorbisfile.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

foreign import CALLCV "vorbis/vorbisfile.h ov_raw_seek"
  ov_raw_seek
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type long}      -- ^ pos
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_pcm_seek"
  ov_pcm_seek
    :: Ptr OggVorbisFile -- ^ vf
    -> Ogg_Int64_t       -- ^ pos
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_time_seek"
  ov_time_seek
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type double}    -- ^ s
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_pcm_seek_page"
  ov_pcm_seek_page
    :: Ptr OggVorbisFile -- ^ vf
    -> Ogg_Int64_t       -- ^ pos
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_time_seek_page"
  ov_time_seek_page
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type double}    -- ^ s
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_raw_seek_lap"
  ov_raw_seek_lap
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type long}      -- ^ pos
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_pcm_seek_lap"
  ov_pcm_seek_lap
    :: Ptr OggVorbisFile -- ^ vf
    -> Ogg_Int64_t       -- ^ pos
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_time_seek_lap"
  ov_time_seek_lap
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type double}    -- ^ s
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_pcm_seek_page_lap"
  ov_pcm_seek_page_lap
    :: Ptr OggVorbisFile -- ^ vf
    -> Ogg_Int64_t       -- ^ pos
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisfile.h ov_time_seek_page_lap"
  ov_time_seek_page_lap
    :: Ptr OggVorbisFile -- ^ vf
    -> #{type double}    -- ^ s
    -> IO #{type int}

