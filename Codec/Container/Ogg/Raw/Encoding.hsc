{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.Encoding where

import           Codec.Container.Ogg.Raw.Encoding.Internal
import           Codec.Container.Ogg.Raw.Internal.Exception
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "ogg/ogg.h"

-- | Throws 'OggError' on non-zero result.
ogg_stream_packetin :: Ptr OggStreamState -> Ptr OggPacket -> IO ()
ogg_stream_packetin os =
  oggError "ogg_stream_packetin" . ogg_stream_packetin' os



-- | Returns 'True' if a page has been completed and returned.
ogg_stream_pageout :: Ptr OggStreamState -> Ptr OggPacket -> IO Bool
ogg_stream_pageout os og = (/= 0) <$> ogg_stream_pageout' os og



-- | Returns 'True' if a page has been completed and returned.
ogg_stream_pageout_fill :: Ptr OggStreamState -> Ptr OggPacket -> #{type int} -> IO Bool
ogg_stream_pageout_fill os og fillbytes =
  (/= 0) <$> ogg_stream_pageout_fill' os og fillbytes



-- | Returns 'True' if remaining packets have been successfully flushed into the page.
ogg_stream_flush :: Ptr OggStreamState -> Ptr OggPacket -> IO Bool
ogg_stream_flush os og = (/= 0) <$> ogg_stream_flush' os og



-- | Returns 'True' if remaining packets have been successfully flushed into the page.
ogg_stream_flush_fill :: Ptr OggStreamState -> Ptr OggPacket -> #{type int} -> IO Bool
ogg_stream_flush_fill os og fillbytes =
  (/= 0) <$> ogg_stream_flush_fill' os og fillbytes
