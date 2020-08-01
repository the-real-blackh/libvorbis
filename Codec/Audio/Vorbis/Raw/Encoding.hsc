{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Encoding where

import           Codec.Audio.Vorbis.Raw.Encoding.Internal
import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Control.Monad (void)
import           Data.Int
import           Data.Maybe (fromMaybe)
import           Foreign.Ptr

#include "vorbis/codec.h"

-- | Throws 'VorbisError' on negative result.
vorbis_analysis :: Ptr VorbisBlock -> Ptr OggPacket -> IO ()
vorbis_analysis vb op =
  ovError "vorbis_analysis" $ vorbis_analysis' vb op



-- | Returns 'True' if more blocks are available.
--
--   Throws 'VorbisError' on negative result.
vorbis_analysis_blockout :: Ptr VorbisDspState -> Ptr VorbisBlock -> IO Bool
vorbis_analysis_blockout vdp vb = do
  res <- vorbis_analysis_blockout' vdp vb
  case res of
    1 -> return True
    0 -> return False
    _ -> ovError' "vorbis_analysis_blockout" res



foreign import ccall unsafe "vorbis_analysis_buffer"
  vorbis_analysis_buffer :: Ptr VorbisDspState -> #{type int} -> IO (Ptr (Ptr #{type float}))



-- | Throws 'VorbisError' on negative result.
vorbis_analysis_headerout
  :: Ptr VorbisDspState -- v
  -> Ptr VorbisComment  -- vc
  -> Ptr OggPacket      -- op
  -> Ptr OggPacket      -- op_comm
  -> Ptr OggPacket      -- op_code
  -> IO ()
vorbis_analysis_headerout v vc op op_comm op_code =
  ovError "vorbis_analysis_headerout" $
    vorbis_analysis_headerout' v vc op op_comm op_code



-- | Result is always zero and thus is dropped.
vorbis_analysis_init :: Ptr VorbisDspState -> Ptr VorbisInfo -> IO ()
vorbis_analysis_init v vi =
  void $ vorbis_analysis_init' v vi



-- | Passing 'Nothing' means all input data has been provided and
--   the compressed stream should be finalized..
--
--   Throws 'VorbisError' on negative result.
vorbis_analysis_wrote :: Ptr VorbisDspState -> Maybe #{type int} -> IO ()
vorbis_analysis_wrote v =
  ovError "vorbis_analysis_wrote" .
    vorbis_analysis_wrote' v . fromMaybe 0



-- | Throws 'VorbisError' on negative result.
vorbis_bitrate_addblock :: Ptr VorbisBlock -> IO ()
vorbis_bitrate_addblock =
  ovError "vorbis_bitrate_addblock" . vorbis_bitrate_addblock'



-- | Returns 'True' if more blocks are available.
--
--   Throws 'VorbisError' on negative result.
vorbis_bitrate_flushpacket :: Ptr VorbisBlock -> Ptr OggPacket -> IO Bool
vorbis_bitrate_flushpacket vd op = do
  res <- vorbis_bitrate_flushpacket' vd op
  case res of
    1 -> return True
    0 -> return False
    _ -> ovError' "vorbis_bitrate_flushpacket" res
