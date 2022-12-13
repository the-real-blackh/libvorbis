{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Vorbisfile.Setup where

import           Vorbisfile.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"
#include "extra/vorbisfile.h"

foreign import CALLCV "vorbis/vorbisfile.h ov_fopen"
  ov_fopen
    :: Ptr #{type char}  -- ^ path
    -> Ptr OggVorbis_File -- ^ vf
    -> IO #type int



foreign import CALLCV "vorbis/vorbisfile.h ov_open"
  ov_open
    :: Ptr ()            -- ^ f
    -> Ptr OggVorbis_File -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> IO #type int



foreign import CALLCV "extra/vorbisfile.h ov_open_callbacks_ptr"
  ov_open_callbacks
    :: Ptr ()            -- ^ datasource
    -> Ptr OggVorbis_File -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> Ptr Ov_callbackss   -- ^ callbacks
    -> IO #type int



foreign import CALLCV "vorbis/vorbisfile.h ov_clear"
  ov_clear
    :: Ptr OggVorbis_File -- ^ vf
    -> IO #type int



foreign import CALLCV "vorbis/vorbisfile.h ov_test"
  ov_test
    :: Ptr ()            -- ^ f
    -> Ptr OggVorbis_File -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> IO #type int



foreign import CALLCV "extra/vorbisfile.h ov_test_callbacks_ptr"
  ov_test_callbacks
    :: Ptr ()            -- ^ datasource
    -> Ptr OggVorbis_File -- ^ vf
    -> Ptr #{type char}  -- ^ initial
    -> #{type long}      -- ^ ibytes
    -> Ptr Ov_callbackss   -- ^ callbacks
    -> IO #type int



foreign import CALLCV "vorbis/vorbisfile.h ov_test_open"
  ov_test_open
    :: Ptr OggVorbis_File -- ^ vf
    -> IO #type int
