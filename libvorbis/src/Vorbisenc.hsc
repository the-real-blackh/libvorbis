{-# LANGUAGE CApiFFI
           , CPP
           , ForeignFunctionInterface
           , PatternSynonyms #-}

module Vorbisenc
  ( -- ** Data Strutcures
    VorbisInfo (..)
  , OvectlRatemanageArg (..)
  , OvectlRatemanage2Arg (..)
    -- ** Encoder Setup
  , vorbis_encode_ctl
  , pattern OV_ECTL_RATEMANAGE2_GET
  , pattern OV_ECTL_RATEMANAGE2_SET
  , pattern OV_ECTL_LOWPASS_GET
  , pattern OV_ECTL_LOWPASS_SET
  , pattern OV_ECTL_IBLOCK_GET
  , pattern OV_ECTL_IBLOCK_SET
  , pattern OV_ECTL_COUPLING_GET
  , pattern OV_ECTL_COUPLING_SET
  , pattern OV_ECTL_RATEMANAGE_GET
  , pattern OV_ECTL_RATEMANAGE_SET
  , pattern OV_ECTL_RATEMANAGE_AVG
  , pattern OV_ECTL_RATEMANAGE_HARD
  , vorbis_encode_init
  , vorbis_encode_init_vbr
  , vorbis_encode_setup_init
  , vorbis_encode_setup_managed
  , vorbis_encode_setup_vbr
    -- ** Return Codes
  , pattern OV_FALSE
  , pattern OV_EOF
  , pattern OV_HOLE
  , pattern OV_EREAD
  , pattern OV_EFAULT
  , pattern OV_EIMPL
  , pattern OV_EINVAL
  , pattern OV_ENOTVORBIS
  , pattern OV_EBADHEADER
  , pattern OV_EVERSION
  , pattern OV_ENOTAUDIO
  , pattern OV_EBADPACKET
  , pattern OV_EBADLINK
  , pattern OV_ENOSEEK
  ) where

import           Libvorbis.Return
import           Libvorbis.Types
import           Vorbisenc.Types

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
      , OV_ECTL_RATEMANAGE_GET
      , OV_ECTL_RATEMANAGE_SET
      , OV_ECTL_RATEMANAGE_AVG
      , OV_ECTL_RATEMANAGE_HARD
     :: (Eq a, Num a) => a
pattern OV_ECTL_RATEMANAGE2_GET = #const OV_ECTL_RATEMANAGE2_GET
pattern OV_ECTL_RATEMANAGE2_SET = #const OV_ECTL_RATEMANAGE2_SET
pattern OV_ECTL_LOWPASS_GET     = #const OV_ECTL_LOWPASS_GET
pattern OV_ECTL_LOWPASS_SET     = #const OV_ECTL_LOWPASS_SET
pattern OV_ECTL_IBLOCK_GET      = #const OV_ECTL_IBLOCK_GET
pattern OV_ECTL_IBLOCK_SET      = #const OV_ECTL_IBLOCK_SET
pattern OV_ECTL_COUPLING_GET    = #const OV_ECTL_COUPLING_GET
pattern OV_ECTL_COUPLING_SET    = #const OV_ECTL_COUPLING_SET
pattern OV_ECTL_RATEMANAGE_GET  = #const OV_ECTL_RATEMANAGE_GET
pattern OV_ECTL_RATEMANAGE_SET  = #const OV_ECTL_RATEMANAGE_SET
pattern OV_ECTL_RATEMANAGE_AVG  = #const OV_ECTL_RATEMANAGE_AVG
pattern OV_ECTL_RATEMANAGE_HARD = #const OV_ECTL_RATEMANAGE_HARD



foreign import CALLCV "vorbis/vorbisenc.h vorbis_encode_ctl"
  vorbis_encode_ctl
    :: Ptr VorbisInfo -- ^ vi
    -> #{type int}    -- ^ request
    -> Ptr ()         -- ^ arg
    -> IO #type int



foreign import CALLCV "vorbis/vorbisenc.h vorbis_encode_init"
  vorbis_encode_init
    :: Ptr VorbisInfo -- ^ vi
    -> #{type long}   -- ^ channels
    -> #{type long}   -- ^ rate
    -> #{type long}   -- ^ max_bitrate
    -> #{type long}   -- ^ nominal_bitrate
    -> #{type long}   -- ^ min_bitrate
    -> IO #type int



foreign import CALLCV "vorbis/vorbisenc.h vorbis_encode_init_vbr"
  vorbis_encode_init_vbr
    :: Ptr VorbisInfo -- ^ vi
    -> #{type long}   -- ^ channels
    -> #{type long}   -- ^ rate
    -> #{type float}  -- ^ base_quality
    -> IO #type int



foreign import CALLCV "vorbis/vorbisenc.h vorbis_encode_setup_init"
  vorbis_encode_setup_init
    :: Ptr VorbisInfo -- ^ vi
    -> IO #{type int}



foreign import CALLCV "vorbis/vorbisenc.h vorbis_encode_setup_managed"
  vorbis_encode_setup_managed
    :: Ptr VorbisInfo -- ^ vi
    -> #{type long}   -- ^ channels
    -> #{type long}   -- ^ rate
    -> #{type long}   -- ^ max_bitrate
    -> #{type long}   -- ^ nominal_bitrate
    -> #{type long}   -- ^ min_bitrate
    -> IO #type int



foreign import CALLCV "vorbis/vorbisenc.h vorbis_encode_setup_vbr"
  vorbis_encode_setup_vbr
    :: Ptr VorbisInfo -- ^ vi
    -> #{type long}   -- ^ channels
    -> #{type long}   -- ^ rate
    -> #{type float}  -- ^ base_quality
    -> IO #type int
