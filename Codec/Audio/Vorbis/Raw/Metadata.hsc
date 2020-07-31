{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Metadata where

import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Audio.Vorbis.Raw.Metadata.Internal
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Control.Exception
import           Data.ByteString.Char8
import           Data.Coerce
import           Data.Int
import           Data.Traversable (for)
import           Foreign.Marshal.Alloc
import           Foreign.Ptr

#include "vorbis/codec.h"

vorbis_comment_add :: Ptr VorbisComment -> ByteString -> IO ()
vorbis_comment_add vc comment =
  useAsCString comment $
    vorbis_comment_add' vc . castPtr



vorbis_comment_add_tag
  :: Ptr VorbisComment
  -> (ByteString, ByteString) -- ^ (tag, contents)
  -> IO ()
vorbis_comment_add_tag vc (tag, contents) =
  useAsCString tag $ \tagPtr ->
    useAsCString contents $ \contentsPtr ->
      vorbis_comment_add_tag' vc (castPtr tagPtr) (castPtr contentsPtr)



foreign import ccall unsafe "vorbis_comment_clear"
  vorbis_comment_clear :: Ptr VorbisComment -> IO ()



foreign import ccall unsafe "vorbis_comment_init"
  vorbis_comment_init :: Ptr VorbisComment -> IO ()



-- | Wrapper over 'vorbis_comment_init' and 'vorbis_comment_clear'. The passed in
--   'VorbisComment' pointer should not be used after this function terminates.
ov_comment_with :: (Ptr VorbisComment -> IO a) -> IO a
ov_comment_with action =
  alloca $ \vf ->
    bracket_ (vorbis_comment_init vf)
             (vorbis_comment_clear vf)
             (action vf)



-- | Returns 'Nothing' instead of 'nullPtr'.
vorbis_comment_query :: Ptr VorbisComment -> ByteString -> #{type int} -> IO (Maybe ByteString)
vorbis_comment_query vc tag n =
  useAsCString tag $ \tagPtr -> do
    res <- vorbis_comment_query' vc (castPtr tagPtr) n
    if res /= nullPtr
      then Just <$> packCString (castPtr res)
      else return Nothing



-- | Returns 'Nothing' instead of @0@.
vorbis_comment_query_count :: Ptr VorbisComment -> ByteString -> IO (Maybe #{type int})
vorbis_comment_query_count vc tag =
  useAsCString tag $ \tagPtr -> do
    res <- vorbis_comment_query_count' vc (castPtr tagPtr)
    return $ case res of
               0 -> Nothing
               _ -> Just res



-- | A wrapper around 'vorbis_comment_query'' and 'vorbis_comment_query_count''.
--   Returns all comments present with the given tag.
vorbis_comments_query :: Ptr VorbisComment -> ByteString -> IO [ByteString]
vorbis_comments_query vc tag =
  useAsCString tag $ \tagPtr -> do
    comms <- vorbis_comment_query_count' vc (coerce tagPtr)
    -- for [1..0] will return an empty array
    for [1..comms] $ \n -> do
      str <- vorbis_comment_query' vc (coerce tagPtr) n
      packCString $ coerce str



-- | Throws 'VorbisError' on negative results.
vorbis_commentheader_out :: Ptr VorbisComment -> Ptr OggPacket -> IO ()
vorbis_commentheader_out vc =
  ovError "vorbis_commentheader_out" .
    vorbis_commentheader_out' vc
