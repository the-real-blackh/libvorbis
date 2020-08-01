{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.Bitpacking where

import           Codec.Container.Ogg.Raw.Bitpacking.Internal
import           Codec.Container.Ogg.Raw.Types

import           Control.Exception
import           Data.Int
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import ccall unsafe "oggpack_writeinit"
  oggpack_writeinit :: Ptr OggpackBuffer -> IO ()



-- | Wrapper over 'oggpack_writeinit' and 'oggpack_writeclear'. The passed in
--   'OggpackBuffer' pointer should not be used after this function terminates.
oggpack_writewith :: (Ptr OggpackBuffer -> IO a) -> IO a
oggpack_writewith action =
  alloca $ \vf ->
    bracket_ (oggpack_writeinit vf)
             (oggpack_writeclear vf)
             (action vf)



-- | Returns 'True' if the buffer is ready for writing.
oggpack_writecheck :: Ptr OggpackBuffer -> IO Bool
oggpack_writecheck b = (== 0) <$> oggpack_writecheck' b



foreign import ccall unsafe "oggpack_reset"
  oggpack_reset :: Ptr OggpackBuffer -> IO ()



foreign import ccall unsafe "oggpack_writetrunc"
  oggpack_writetrunc :: Ptr OggpackBuffer -> #{type long} -> IO ()



foreign import ccall unsafe "oggpack_writealign"
  oggpack_writealign :: Ptr OggpackBuffer -> IO ()



foreign import ccall unsafe "oggpack_writecopy"
  oggpack_writecopy :: Ptr OggpackBuffer -> Ptr () -> #{type long} -> IO ()



foreign import ccall unsafe "oggpack_writeclear"
  oggpack_writeclear :: Ptr OggpackBuffer -> IO ()



foreign import ccall unsafe "oggpack_readinit"
  oggpack_readinit :: Ptr OggpackBuffer -> Ptr #{type unsigned char} -> #{type int} -> IO ()



foreign import ccall unsafe "oggpack_write"
  oggpack_write :: Ptr OggpackBuffer -> #{type unsigned long} -> #{type int} -> IO ()



foreign import ccall unsafe "oggpack_look"
  oggpack_look :: Ptr OggpackBuffer -> #{type int} -> IO #type long



foreign import ccall unsafe "oggpack_look1"
  oggpack_look1 :: Ptr OggpackBuffer -> IO #type long



foreign import ccall unsafe "oggpack_adv"
  oggpack_adv :: Ptr OggpackBuffer -> #{type int} -> IO ()



foreign import ccall unsafe "oggpack_adv1"
  oggpack_adv1 :: Ptr OggpackBuffer -> IO ()



foreign import ccall unsafe "oggpack_read"
  oggpack_read :: Ptr OggpackBuffer -> #{type int} -> IO #type long



foreign import ccall unsafe "oggpack_read1"
  oggpack_read1 :: Ptr OggpackBuffer -> IO #type long



foreign import ccall unsafe "oggpack_bytes"
  oggpack_bytes :: Ptr OggpackBuffer -> IO #type long



foreign import ccall unsafe "oggpack_bits"
  oggpack_bits :: Ptr OggpackBuffer -> IO #type long



foreign import ccall unsafe "oggpack_get_buffer"
  oggpack_get_buffer :: Ptr OggpackBuffer -> IO (Ptr #{type unsigned char})
