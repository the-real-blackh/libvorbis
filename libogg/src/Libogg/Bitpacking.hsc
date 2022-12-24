{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libogg.Bitpacking where

import           Libogg.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "ogg/ogg.h"

foreign import CALLCV "ogg/ogg.h oggpack_writeinit"
  oggpack_writeinit
    :: Ptr Oggpack_buffer -- ^ b
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_writecheck"
  oggpack_writecheck
    :: Ptr Oggpack_buffer -- ^ b
    -> IO #type int



foreign import CALLCV "ogg/ogg.h oggpack_reset"
  oggpack_reset
    :: Ptr Oggpack_buffer -- ^ b
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_writetrunc"
  oggpack_writetrunc
    :: Ptr Oggpack_buffer -- ^ b
    -> #{type long}      -- ^ bits
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_writealign"
  oggpack_writealign
    :: Ptr Oggpack_buffer -- ^ b
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_writecopy"
  oggpack_writecopy
    :: Ptr Oggpack_buffer -- ^ b
    -> Ptr ()            -- ^ source
    -> #{type long}      -- ^ bits
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_writeclear"
  oggpack_writeclear
    :: Ptr Oggpack_buffer -- ^ b
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_readinit"
  oggpack_readinit
    :: Ptr Oggpack_buffer         -- ^ b
    -> Ptr #{type unsigned char} -- ^ buf
    -> #{type int}               -- ^ bytes
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_write"
  oggpack_write
    :: Ptr Oggpack_buffer     -- ^ b
    -> #{type unsigned long} -- ^ value
    -> #{type int}           -- ^ bits
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_look"
  oggpack_look
    :: Ptr Oggpack_buffer -- ^ b
    -> #{type int}       -- ^ bits
    -> IO #type long



foreign import CALLCV "ogg/ogg.h oggpack_look1"
  oggpack_look1
    :: Ptr Oggpack_buffer -- ^ b
    -> IO #type long



foreign import CALLCV "ogg/ogg.h oggpack_adv"
  oggpack_adv
    :: Ptr Oggpack_buffer -- ^ b
    -> #{type int}       -- ^ bits
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_adv1"
  oggpack_adv1
    :: Ptr Oggpack_buffer -- ^ b
    -> IO ()



foreign import CALLCV "ogg/ogg.h oggpack_read"
  oggpack_read
    :: Ptr Oggpack_buffer -- ^ b
    -> #{type int}       -- ^ bits
    -> IO #type long



foreign import CALLCV "ogg/ogg.h oggpack_read1"
  oggpack_read1
    :: Ptr Oggpack_buffer -- ^ b
    -> IO #type long



foreign import CALLCV "ogg/ogg.h oggpack_bytes"
  oggpack_bytes
    :: Ptr Oggpack_buffer -- ^ b
    -> IO #type long



foreign import CALLCV "ogg/ogg.h oggpack_bits"
  oggpack_bits
    :: Ptr Oggpack_buffer -- ^ b
    -> IO #type long



foreign import CALLCV "ogg/ogg.h oggpack_get_buffer"
  oggpack_get_buffer
    :: Ptr Oggpack_buffer              -- ^ b
    -> IO (Ptr #{type unsigned char})
