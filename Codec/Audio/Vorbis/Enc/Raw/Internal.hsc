{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module Codec.Audio.Vorbis.Enc.Raw.Internal where

import           Codec.Audio.Vorbis.Raw.Types

import           Data.Int
import           Foreign.Ptr

#include "vorbis/vorbisenc.h"

pattern OV_ECTL_RATEMANAGE2_GET
      , OV_ECTL_RATEMANAGE2_SET
      , OV_ECTL_LOWPASS_GET
      , OV_ECTL_LOWPASS_SET
      , OV_ECTL_IBLOCK_GET
      , OV_ECTL_IBLOCK_SET
      , OV_ECTL_COUPLING_GET
      , OV_ECTL_COUPLING_SET
     :: (Eq a, Num a) => a
pattern OV_ECTL_RATEMANAGE2_GET = #const OV_ECTL_RATEMANAGE2_GET
pattern OV_ECTL_RATEMANAGE2_SET = #const OV_ECTL_RATEMANAGE2_SET
pattern OV_ECTL_LOWPASS_GET     = #const OV_ECTL_LOWPASS_GET
pattern OV_ECTL_LOWPASS_SET     = #const OV_ECTL_LOWPASS_SET
pattern OV_ECTL_IBLOCK_GET      = #const OV_ECTL_IBLOCK_GET
pattern OV_ECTL_IBLOCK_SET      = #const OV_ECTL_IBLOCK_SET
pattern OV_ECTL_COUPLING_GET    = #const OV_ECTL_COUPLING_GET
pattern OV_ECTL_COUPLING_SET    = #const OV_ECTL_COUPLING_SET



foreign import ccall unsafe "vorbis_encode_ctl"
  vorbis_encode_ctl' :: Ptr VorbisInfo -> #{type int} -> Ptr () -> IO #type int



foreign import ccall unsafe "vorbis_encode_init"
  vorbis_encode_init'
    :: Ptr VorbisInfo
    -> #type long
    -> #type long
    -> #type long
    -> #type long
    -> #type long
    -> IO #type int



foreign import ccall unsafe "vorbis_encode_init_vbr"
  vorbis_encode_init_vbr'
    :: Ptr VorbisInfo
    -> #type long
    -> #type long
    -> #type float
    -> IO #type int



foreign import ccall unsafe "vorbis_encode_setup_init"
  vorbis_encode_setup_init' :: Ptr VorbisInfo -> IO #{type int}



foreign import ccall unsafe "vorbis_encode_setup_managed"
  vorbis_encode_setup_managed'
    :: Ptr VorbisInfo
    -> #type long
    -> #type long
    -> #type long
    -> #type long
    -> #type long
    -> IO #type int



foreign import ccall unsafe "vorbis_encode_setup_vbr"
  vorbis_encode_setup_vbr'
    :: Ptr VorbisInfo
    -> #type long
    -> #type long
    -> #type float
    -> IO #type int
