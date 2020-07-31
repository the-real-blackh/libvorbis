{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.General where

import           Codec.Container.Ogg.Raw.Internal.Exception
import           Codec.Container.Ogg.Raw.General.Internal
import           Codec.Container.Ogg.Raw.Types

import           Control.Exception
import           Control.Monad
import           Data.Int
import           Foreign.Marshal.Alloc
import           Foreign.Ptr

#include "ogg/ogg.h"

-- | Throws 'OggError' on non-zero result.
ogg_stream_init :: Ptr OggStreamState -> #{type int} -> IO ()
ogg_stream_init os =
  oggError "ogg_stream_init" . ogg_stream_init' os



-- | Wrapper over 'ogg_stream_init' and 'ogg_stream_clear'. The passed in
--   'OggStreamState' pointer should not be used after this function terminates.
ogg_stream_with :: #{type int} -> (Ptr OggStreamState -> IO a) -> IO a
ogg_stream_with serialno action =
  alloca $ \vf ->
    bracket_ (ogg_stream_init vf serialno)
             (ogg_stream_clear vf)
             (action vf)

-- | Throws 'OggError' on non-zero result.
ogg_stream_check :: Ptr OggStreamState -> IO ()
ogg_stream_check =
  oggError "ogg_stream_check" . ogg_stream_check'



-- | Result is always zero and thus is dropped.
ogg_stream_clear :: Ptr OggStreamState -> IO ()
ogg_stream_clear = void . ogg_stream_clear'



-- | Throws 'OggError' on non-zero result.
ogg_stream_reset :: Ptr OggStreamState -> IO ()
ogg_stream_reset =
  oggError "ogg_stream_reset" . ogg_stream_reset'



-- | Throws 'OggError' on non-zero result.
ogg_stream_reset_serialno :: Ptr OggStreamState -> #{type int} -> IO ()
ogg_stream_reset_serialno os =
  oggError "ogg_stream_reset_serialno" . ogg_stream_reset_serialno' os



-- | Result is always zero and thus is dropped.
ogg_stream_destroy :: Ptr OggStreamState -> IO ()
ogg_stream_destroy = void . ogg_stream_destroy'



-- | Throws 'OggError' on non-zero result.
ogg_page_version :: Ptr OggPage -> IO ()
ogg_page_version = 
  oggError "ogg_page_version" . ogg_page_version'



-- | Returns 'True' if this page contains packet data continued from the last page.
ogg_page_continued :: Ptr OggPage -> IO Bool
ogg_page_continued og = do
  res <- ogg_page_continued' og
  case res of
    1 -> return True
    0 -> return False
    _ -> oggError' "ogg_page_continued" res -- Unreachable



foreign import ccall unsafe "ogg_page_packets"
  ogg_page_packets :: Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_page_bos"
  ogg_page_bos :: Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_page_eos"
  ogg_page_eos :: Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_page_granulepos"
  ogg_page_granulepos :: Ptr OggPage -> IO Ogg_Int64_t



foreign import ccall unsafe "ogg_page_serialno"
  ogg_page_serialno :: Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_page_pageno"
  ogg_page_pageno :: Ptr OggPage -> IO #type long



foreign import ccall unsafe "ogg_packet_clear"
  ogg_packet_clear :: Ptr OggPacket -> IO ()



foreign import ccall unsafe "ogg_page_checksum_set"
  ogg_page_checksum_set :: Ptr OggPage -> IO ()
