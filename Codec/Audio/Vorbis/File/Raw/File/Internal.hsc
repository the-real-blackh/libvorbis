{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.File.Raw.File.Internal where

import           Codec.Audio.Vorbis.File.Raw.Types
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

foreign import ccall "ov_bitrate"
  ov_bitrate' :: Ptr (OggVorbisFile a) -> #{type int} -> IO #type long



foreign import ccall "ov_bitrate_instant"
  ov_bitrate_instant' :: Ptr (OggVorbisFile a) -> IO #type long



foreign import ccall "ov_seekable"
  ov_seekable' :: Ptr (OggVorbisFile a) -> IO #type long



foreign import ccall "ov_serialnumber"
  ov_serialnumber' :: Ptr (OggVorbisFile a) -> #{type int} -> IO #type long



foreign import ccall "ov_raw_total"
  ov_raw_total' :: Ptr (OggVorbisFile a) -> #{type int} -> IO Ogg_Int64_t



foreign import ccall "ov_pcm_total"
  ov_pcm_total' :: Ptr (OggVorbisFile a) -> #{type int} -> IO Ogg_Int64_t



foreign import ccall "ov_time_total"
  ov_time_total' :: Ptr (OggVorbisFile a) -> #{type int} -> IO #type double



foreign import ccall "ov_raw_tell"
  ov_raw_tell' :: Ptr (OggVorbisFile a) -> IO Ogg_Int64_t



foreign import ccall "ov_pcm_tell"
  ov_pcm_tell' :: Ptr (OggVorbisFile a) -> IO Ogg_Int64_t



foreign import ccall "ov_time_tell"
  ov_time_tell' :: Ptr (OggVorbisFile a) -> IO #type double



foreign import ccall "ov_info"
  ov_info' :: Ptr (OggVorbisFile a) -> #{type int} -> IO (Ptr VorbisInfo)



foreign import ccall "ov_comment"
  ov_comment' :: Ptr (OggVorbisFile a) -> #{type int} -> IO (Ptr VorbisComment)
