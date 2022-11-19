{-# LANGUAGE ForeignFunctionInterface #-}

module Vorbisfile.Setup where

import           Vorbisfile.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

foreign import ccall "ov_fopen"
  ov_fopen
    :: Ptr #{type char}  -- ^ path
    -> Ptr OggVorbisFile -- ^ vf
    -> IO #type int



foreign import ccall "ov_open"
  ov_open
    :: Ptr ()            -- ^ f
    -> Ptr OggVorbisFile -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> IO #type int



foreign import ccall "ov_open_callbacks_plus"
  ov_open_callbacks
    :: Ptr ()            -- ^ datasource
    -> Ptr OggVorbisFile -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> Ptr OvCallbacks   -- ^ callbacks
    -> IO #type int



foreign import ccall "ov_clear"
  ov_clear
    :: Ptr OggVorbisFile -- ^ vf
    -> IO #type int



foreign import ccall "ov_test"
  ov_test
    :: Ptr ()            -- ^ f
    -> Ptr OggVorbisFile -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> IO #type int



foreign import ccall "ov_test_callbacks_plus"
  ov_test_callbacks
    :: Ptr ()            -- ^ datasource
    -> Ptr OggVorbisFile -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> Ptr OvCallbacks   -- ^ callbacks
    -> IO #type int



foreign import ccall "ov_test_open"
  ov_test_open
    :: Ptr OggVorbisFile -- ^ vf
    -> IO #type int
