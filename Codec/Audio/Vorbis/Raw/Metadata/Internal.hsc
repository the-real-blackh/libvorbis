{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Metadata.Internal where

import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import ccall unsafe "vorbis_comment_add"
  vorbis_comment_add' :: Ptr VorbisComment -> Ptr #{type char} -> IO ()



foreign import ccall unsafe "vorbis_comment_add_tag"
  vorbis_comment_add_tag' :: Ptr VorbisComment -> Ptr #{type char} -> Ptr #{type char} -> IO ()



foreign import ccall unsafe "vorbis_comment_query"
  vorbis_comment_query' :: Ptr VorbisComment -> Ptr #{type char} -> #{type int} -> IO (Ptr #{type char})



foreign import ccall unsafe "vorbis_comment_query_count"
  vorbis_comment_query_count' :: Ptr VorbisComment -> Ptr #{type char} -> IO #type int




foreign import ccall unsafe "vorbis_commentheader_out"
  vorbis_commentheader_out' :: Ptr VorbisComment -> Ptr OggPacket -> IO #type int
