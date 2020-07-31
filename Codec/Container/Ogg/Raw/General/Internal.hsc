{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.General.Internal where

import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr



foreign import ccall unsafe "ogg_stream_init"
  ogg_stream_init' :: Ptr OggStreamState -> #{type int} -> IO #type int



foreign import ccall unsafe "ogg_stream_check"
  ogg_stream_check' :: Ptr OggStreamState -> IO #type int



foreign import ccall unsafe "ogg_stream_clear"
  ogg_stream_clear' :: Ptr OggStreamState -> IO #type int



foreign import ccall unsafe "ogg_stream_reset"
  ogg_stream_reset' :: Ptr OggStreamState -> IO #type int



foreign import ccall unsafe "ogg_stream_reset_serialno"
  ogg_stream_reset_serialno' :: Ptr OggStreamState -> #{type int} -> IO #type int



foreign import ccall unsafe "ogg_stream_destroy"
  ogg_stream_destroy' :: Ptr OggStreamState -> IO #type int



foreign import ccall unsafe "ogg_page_version"
  ogg_page_version' :: Ptr OggPage -> IO #type int



foreign import ccall unsafe "ogg_page_continued"
  ogg_page_continued' :: Ptr OggPage -> IO #type int
