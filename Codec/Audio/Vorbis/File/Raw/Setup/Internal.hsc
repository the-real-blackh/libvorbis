{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.File.Raw.Setup.Internal where

import           Codec.Audio.Vorbis.File.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"
#include "vorbis/vorbisfile-plus.h"

foreign import ccall unsafe "ov_fopen"
  ov_fopen' :: Ptr #{type char} -> Ptr (OggVorbisFile ()) -> IO #type int



#ifndef windows_HOST_OS
foreign import ccall unsafe "ov_open"
  ov_open' :: Ptr () -> Ptr (OggVorbisFile ()) -> Ptr #{type char} -> #{type long} -> IO #type int
#endif



foreign import ccall "ov_open_callbacks_plus"
  ov_open_callbacks' :: Ptr a -> Ptr (OggVorbisFile a) -> Ptr #{type char} -> #{type long} -> Ptr (OvCallbacks a) -> IO #type int



foreign import ccall unsafe "ov_clear"
  ov_clear' :: Ptr (OggVorbisFile a) -> IO #type int



foreign import ccall unsafe "ov_test"
  ov_test' :: Ptr () -> Ptr (OggVorbisFile ()) -> Ptr #{type char} -> #{type long} -> IO #type int



foreign import ccall "ov_test_callbacks_plus"
  ov_test_callbacks' :: Ptr a -> Ptr (OggVorbisFile a) -> Ptr #{type char} -> #{type long} -> Ptr (OvCallbacks a) -> IO #type int



foreign import ccall unsafe "ov_test_open"
  ov_test_open' :: Ptr (OggVorbisFile a) -> IO #type int
