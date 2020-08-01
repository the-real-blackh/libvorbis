{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.File.Raw.Seeking.Internal where

import           Codec.Audio.Vorbis.File.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

foreign import ccall "ov_raw_seek"
  ov_raw_seek' :: Ptr (OggVorbisFile a) -> #{type long} -> IO #{type int}



foreign import ccall "ov_pcm_seek"
  ov_pcm_seek' :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO #{type int}



foreign import ccall "ov_time_seek"
  ov_time_seek' :: Ptr (OggVorbisFile a) -> #{type double} -> IO #{type int}



foreign import ccall "ov_pcm_seek_page"
  ov_pcm_seek_page' :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO #{type int}



foreign import ccall "ov_time_seek_page"
  ov_time_seek_page' :: Ptr (OggVorbisFile a) -> #{type double} -> IO #{type int}



foreign import ccall "ov_raw_seek_lap"
  ov_raw_seek_lap' :: Ptr (OggVorbisFile a) -> #{type long} -> IO #{type int}



foreign import ccall "ov_pcm_seek_lap"
  ov_pcm_seek_lap' :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO #{type int}



foreign import ccall "ov_time_seek_lap"
  ov_time_seek_lap' :: Ptr (OggVorbisFile a) -> #{type double} -> IO #{type int}



foreign import ccall "ov_pcm_seek_page_lap"
  ov_pcm_seek_page_lap' :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO #{type int}



foreign import ccall "ov_time_seek_page_lap"
  ov_time_seek_page_lap' :: Ptr (OggVorbisFile a) -> #{type double} -> IO #{type int}
