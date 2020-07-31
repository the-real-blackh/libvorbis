{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.Bitpacking.Internal where

import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import ccall unsafe "oggpack_writecheck"
  oggpack_writecheck' :: Ptr OggpackBuffer -> IO #type int
