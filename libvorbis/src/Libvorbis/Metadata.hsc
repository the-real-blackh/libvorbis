{-# LANGUAGE ForeignFunctionInterface #-}

module Libvorbis.Metadata where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import ccall "vorbis_comment_add"
  vorbis_comment_add
    :: Ptr VorbisComment -- ^ vc
    -> Ptr #{type char}  -- ^ comment
    -> IO ()



foreign import ccall "vorbis_comment_add_tag"
  vorbis_comment_add_tag
    :: Ptr VorbisComment -- ^ vc
    -> Ptr #{type char}  -- ^ tag
    -> Ptr #{type char}  -- ^ contents
    -> IO ()



foreign import ccall "vorbis_comment_clear"
  vorbis_comment_clear
    :: Ptr VorbisComment -- ^ vc
    -> IO ()



foreign import ccall "vorbis_comment_init"
  vorbis_comment_init
    :: Ptr VorbisComment -- ^ vc
    -> IO ()



foreign import ccall "vorbis_comment_query"
  vorbis_comment_query
    :: Ptr VorbisComment     -- ^ vc
    -> Ptr #{type char}      -- ^ tag
    -> #{type int}           -- ^ count
    -> IO (Ptr #{type char})



foreign import ccall "vorbis_comment_query_count"
  vorbis_comment_query_count
    :: Ptr VorbisComment -- ^ vc
    -> Ptr #{type char}  -- ^ tag
    -> IO #type int




foreign import ccall "vorbis_commentheader_out"
  vorbis_commentheader_out
    :: Ptr VorbisComment -- ^ vc
    -> Ptr OggPacket     -- ^ op
    -> IO #type int
