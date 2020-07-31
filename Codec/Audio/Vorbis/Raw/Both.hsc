{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Both where

import           Codec.Audio.Vorbis.Raw.Both.Internal
import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Control.Monad (void)
import           Control.Exception
import           Data.Bool (bool)
import           Data.ByteString.Char8
import           Data.Coerce
import           Data.Int
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           System.IO.Unsafe

#include "vorbis/codec.h"

-- | Result is always zero and thus is dropped.
vorbis_block_clear :: Ptr VorbisBlock -> IO ()
vorbis_block_clear = void . vorbis_block_clear'



-- | Result is always zero and thus is dropped.
vorbis_block_init :: Ptr VorbisDspState -> Ptr VorbisBlock -> IO ()
vorbis_block_init v = void . vorbis_block_init' v



-- | Wrapper over 'vorbis_block_init' and 'vorbis_block_clear'. The passed in
--   'VorbisBlock' pointer should not be used after this function terminates.
vorbis_block_with :: Ptr VorbisDspState -> (Ptr VorbisBlock -> IO a) -> IO a
vorbis_block_with v action =
  alloca $ \vb ->
    bracket_ (vorbis_block_init v vb)
             (vorbis_block_clear vb)
             (action vb)



foreign import ccall unsafe "vorbis_dsp_clear"
  vorbis_dsp_clear :: Ptr VorbisDspState -> IO ()



-- | Returns 'Nothing' instead of @-1@.
vorbis_granule_time :: Ptr VorbisDspState -> Ogg_Int64_t -> IO (Maybe #type double)
vorbis_granule_time v granulepos = do
  res <- vorbis_granule_time' v granulepos
  return $ case res of
             -1 -> Nothing
             _  -> Just res



-- | Throws 'VorbisError' on negative result.
vorbis_info_blocksize :: Ptr VorbisInfo -> Bool -> IO #type int
vorbis_info_blocksize vi zo = do
  res <- vorbis_info_blocksize' vi $ bool 0 1 zo
  if res > 0
    then ovError' "vorbis_info_blocksize" res
    else return res



foreign import ccall unsafe "vorbis_info_clear"
  vorbis_info_clear :: Ptr VorbisInfo -> IO ()



foreign import ccall unsafe "vorbis_info_init"
  vorbis_info_init :: Ptr VorbisInfo -> IO ()



-- | Wrapper over 'vorbis_info_init' and 'vorbis_info_clear'. The passed in
--   'VorbisInfo' pointer should not be used after this function terminates.
vorbis_info_with :: (Ptr VorbisInfo -> IO a) -> IO a
vorbis_info_with action =
  alloca $ \vb ->
    bracket_ (vorbis_info_init vb)
             (vorbis_info_clear vb)
             (action vb)


-- | Stored in library's static storage, therefore pure.
vorbis_version_string :: ByteString
vorbis_version_string =
  unsafePerformIO . packCString . coerce $ vorbis_version_string'
