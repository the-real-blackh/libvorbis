{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Audio.Vorbis.File.Raw.Decoding.Internal where

import           Codec.Audio.Vorbis.File.Raw.Types

import           Data.Int
import           Data.Proxy
import           Data.Word
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

data VorbisSign = Int
                | Word

data VorbisSize = Eight
                | Sixteen

class VorbisReadFormat v where
  vorbisReadFormat :: Proxy v -> (VorbisSign, VorbisSize)

instance VorbisReadFormat Int8 where
  vorbisReadFormat _ = (Int, Eight)

instance VorbisReadFormat Int16 where
  vorbisReadFormat _ = (Int, Sixteen)

instance VorbisReadFormat Word8 where
  vorbisReadFormat _ = (Word, Eight)

instance VorbisReadFormat Word16 where
  vorbisReadFormat _ = (Word, Sixteen)



foreign import ccall "ov_read"
  ov_read'
    :: Ptr (OggVorbisFile a)
    -> Ptr #type char
    -> #type int
    -> #type int
    -> #type int
    -> #type int
    -> Ptr #type int
    -> IO #type long



foreign import ccall "ov_read_float"
  ov_read_float'
    :: Ptr (OggVorbisFile a)
    -> Ptr (Ptr (Ptr #{type float}))
    -> #type int
    -> Ptr #type int
    -> IO #type long




type FilterFunc a = Ptr (Ptr #{type float})
                 -> #{type long}
                 -> #{type long}
                 -> Ptr a
                 -> IO (Ptr ())

foreign import ccall unsafe "wrapper"
  mkFilterFunc :: FilterFunc () -> IO (FunPtr (FilterFunc ()))

foreign import ccall "ov_read_filter"
  ov_read_filter'
    :: Ptr (OggVorbisFile a)
    -> Ptr #type char
    -> #type int
    -> #type int
    -> #type int
    -> #type int
    -> Ptr #type int
    -> FunPtr (FilterFunc b)
    -> Ptr b
    -> IO #type long




foreign import ccall unsafe "ov_crosslap"
 ov_crosslap' :: Ptr (OggVorbisFile a) -> Ptr (OggVorbisFile b) -> IO #type long
