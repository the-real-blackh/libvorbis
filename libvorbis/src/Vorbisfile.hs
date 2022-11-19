{-# LANGUAGE PatternSynonyms #-}

module Vorbisfile
  ( -- ** Data Structures
    Ogg_Int64_t
  , OggVorbisFile (..)
  , ReadFunc
  , SeekFunc
  , CloseFunc
  , TellFunc
  , OvCallbacks (..)
  , poke_OV_CALLBACKS_DEFAULT
  , poke_OV_CALLBACKS_NOCLOSE
  , poke_OV_CALLBACKS_STREAMONLY
  , poke_OV_CALLBACKS_STREAMONLY_NOCLOSE
  , VorbisComment (..)
  , VorbisInfo (..)
    -- ** Setup/Teardown
  , ov_fopen
  , ov_open
  , ov_open_callbacks
  , ov_clear
  , ov_test
  , ov_test_callbacks
  , ov_test_open
    -- ** Decoding
  , ov_read
  , ov_read_float
  , FilterFunc
  , ov_read_filter
  , ov_crosslap
    -- ** Seeking
  , ov_raw_seek
  , ov_pcm_seek
  , ov_time_seek
  , ov_pcm_seek_page
  , ov_time_seek_page
  , ov_raw_seek_lap
  , ov_pcm_seek_lap
  , ov_time_seek_lap
  , ov_pcm_seek_page_lap
  , ov_time_seek_page_lap
    -- ** File Information
  , ov_bitrate
  , ov_bitrate_instant
  , ov_streams
  , ov_seekable
  , ov_serialnumber
  , ov_raw_total
  , ov_pcm_total
  , ov_time_total
  , ov_raw_tell
  , ov_pcm_tell
  , ov_time_tell
  , ov_info
  , ov_comment
  ) where

import           Libogg
import           Libvorbis.Types
import           Vorbisfile.Decoding
import           Vorbisfile.File
import           Vorbisfile.Seeking
import           Vorbisfile.Setup
import           Vorbisfile.Types
