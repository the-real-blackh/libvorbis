{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface #-}

module Libvorbis.Metadata where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr

#include "vorbis/codec.h"

foreign import CALLCV "vorbis/codec.h vorbis_comment_add"
  vorbis_comment_add
    :: Ptr Vorbis_comment -- ^ vc
    -> Ptr #{type char}  -- ^ comment
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_comment_add_tag"
  vorbis_comment_add_tag
    :: Ptr Vorbis_comment -- ^ vc
    -> Ptr #{type char}  -- ^ tag
    -> Ptr #{type char}  -- ^ contents
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_comment_clear"
  vorbis_comment_clear
    :: Ptr Vorbis_comment -- ^ vc
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_comment_init"
  vorbis_comment_init
    :: Ptr Vorbis_comment -- ^ vc
    -> IO ()



foreign import CALLCV "vorbis/codec.h vorbis_comment_query"
  vorbis_comment_query
    :: Ptr Vorbis_comment     -- ^ vc
    -> Ptr #{type char}      -- ^ tag
    -> #{type int}           -- ^ count
    -> IO (Ptr #{type char})



foreign import CALLCV "vorbis/codec.h vorbis_comment_query_count"
  vorbis_comment_query_count
    :: Ptr Vorbis_comment -- ^ vc
    -> Ptr #{type char}  -- ^ tag
    -> IO #type int




foreign import CALLCV "vorbis/codec.h vorbis_commentheader_out"
  vorbis_commentheader_out
    :: Ptr Vorbis_comment -- ^ vc
    -> Ptr Ogg_packet     -- ^ op
    -> IO #type int
