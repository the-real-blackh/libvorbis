module Codec.Audio.Vorbis.Enc.Raw
  ( -- * Data Structures
    module Codec.Audio.Vorbis.Enc.Raw.Types
    -- ** Data Structures from "Codec.Audio.Vorbis.Raw.Types"
  , VorbisInfo (..)
    -- * Encoder Setup
  , module Codec.Audio.Vorbis.Enc.Raw
  ) where

import           Codec.Audio.Vorbis.Enc.Raw.Internal
import           Codec.Audio.Vorbis.Enc.Raw.Types
import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Audio.Vorbis.Raw.Types

import           Data.Int
import           Data.Maybe (fromMaybe)
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable



-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_ECTL_RATEMANAGE2_GET' passed as @arg@.
--
--   Throws 'VorbisError' on negative result.
vorbis_ratemanage2_get :: Ptr VorbisInfo -> IO OvectlRatemanage2Arg
vorbis_ratemanage2_get vi =
  alloca $ \argPtr -> do
    ovError "vorbis_ratemanage2_get" $
      vorbis_encode_ctl' vi OV_ECTL_RATEMANAGE2_GET (castPtr argPtr)
    peek argPtr

-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_ECTL_RATEMANAGE2_SET' passed as @arg@.
--
--   Passing 'Nothing' disables bitrate management.
--
--   Throws 'VorbisError' on negative result.
vorbis_ratemanage2_set :: Ptr VorbisInfo -> Maybe OvectlRatemanage2Arg -> IO ()
vorbis_ratemanage2_set vi mayArg =
  ovError "vorbis_ratemanage2_set" $
    case mayArg of
      Nothing  -> vorbis_encode_ctl' vi OV_ECTL_RATEMANAGE2_SET nullPtr
      Just arg -> with arg $
                    vorbis_encode_ctl' vi OV_ECTL_RATEMANAGE2_SET . castPtr

-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_ECTL_LOWPASS_GET' passed as @arg@.
--
--   Throws 'VorbisError' on negative result.
vorbis_lowpass_get :: Ptr VorbisInfo -> IO #type double
vorbis_lowpass_get vi =
  alloca $ \argPtr -> do
    ovError "vorbis_lowpass_get" $
      vorbis_encode_ctl' vi OV_ECTL_LOWPASS_GET (castPtr argPtr)
    peek argPtr

-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_ECTL_LOWPASS_SET' passed as @arg@.
--
--   Throws 'VorbisError' on negative result.
vorbis_lowpass_set :: Ptr VorbisInfo -> Double -> IO ()
vorbis_lowpass_set vi arg =
  ovError "vorbis_lowpass_set" .
    with arg $
      vorbis_encode_ctl' vi OV_ECTL_LOWPASS_SET . castPtr

-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_IBLOCK_GET' passed as @arg@.
--
--   Throws 'VorbisError' on negative result.
vorbis_iblock_get :: Ptr VorbisInfo -> IO #type double
vorbis_iblock_get vi =
  alloca $ \argPtr -> do
    ovError "vorbis_iblock_get" $
      vorbis_encode_ctl' vi OV_ECTL_IBLOCK_GET (castPtr argPtr)
    peek argPtr

-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_ECTL_IBLOCK_SET' passed as @arg@.
--
--   Throws 'VorbisError' on negative result.
vorbis_iblock_set :: Ptr VorbisInfo -> Double -> IO ()
vorbis_iblock_set vi arg =
  ovError "vorbis_iblock_set" .
    with arg $
      vorbis_encode_ctl' vi OV_ECTL_IBLOCK_SET . castPtr

-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_ECTL_COUPLING_GET' passed as @arg@.
--
--   Throws 'VorbisError' on negative result.
vorbis_coupling_get :: Ptr VorbisInfo -> IO #type double
vorbis_coupling_get vi =
  alloca $ \argPtr -> do
    ovError "vorbis_coupling_get" $
      vorbis_encode_ctl' vi OV_ECTL_COUPLING_GET (castPtr argPtr)
    peek argPtr

-- | Wrapper over 'vorbis_encode_ctl'' with 'OV_ECTL_RATEMANAGE2_SET' passed as @arg@.
--
--   Passing 'Nothing' disables channel coupling.
--
--   Throws 'VorbisError' on negative result.
vorbis_coupling_set :: Ptr VorbisInfo -> Maybe Double -> IO ()
vorbis_coupling_set vi mayArg =
  ovError "vorbis_coupling_set" $
    case mayArg of
      Nothing -> vorbis_encode_ctl' vi OV_ECTL_COUPLING_SET nullPtr
      Just arg -> with arg $
                    vorbis_encode_ctl' vi OV_ECTL_COUPLING_SET . castPtr



-- | Passing 'Nothing' indicates an unset argument.
--
--   Throws 'VorbisError' on negative result.
vorbis_encode_init
 :: Ptr VorbisInfo
 -> #{type long}       -- ^ channels
 -> #{type long}       -- ^ rate
 -> Maybe #{type long} -- ^ max_bitrate
 -> Maybe #{type long} -- ^ nominal_bitrate
 -> Maybe #{type long} -- ^ min_bitrate
 -> IO ()
vorbis_encode_init vi chanels rat mayMax_bitrat mayNominal_bitrat mayMin_bitrat=
  ovError "vorbis_encode_init" $
    vorbis_encode_init' vi chanels rat
      (fromMaybe (-1) mayMax_bitrat)
      (fromMaybe (-1) mayNominal_bitrat)
      (fromMaybe (-1) mayMin_bitrat)



-- | Throws 'VorbisError' on negative result.
vorbis_encode_init_vbr
  :: Ptr VorbisInfo
  -> #{type long}  -- ^ channels
  -> #{type long}  -- ^ rate
  -> #{type float} -- ^ base_quality
  -> IO ()
vorbis_encode_init_vbr vi chanels rat base_quality =
  ovError "vorbis_encode_init_vbr" $
    vorbis_encode_init_vbr' vi chanels rat base_quality



-- | Throws 'VorbisError' on negative result.
vorbis_encode_setup_init :: Ptr VorbisInfo -> IO ()
vorbis_encode_setup_init vi =
  ovError "vorbis_encode_setup_init" $
    vorbis_encode_setup_init' vi



-- | Passing 'Nothing' indicates an unset argument.
--
--   Throws 'VorbisError' on negative result.
vorbis_encode_setup_managed
 :: Ptr VorbisInfo
 -> #{type long}       -- ^ channels
 -> #{type long}       -- ^ rate
 -> Maybe #{type long} -- ^ max_bitrate
 -> Maybe #{type long} -- ^ nominal_bitrate
 -> Maybe #{type long} -- ^ min_bitrate
 -> IO ()
vorbis_encode_setup_managed vi chanels rat mayMax_bitrat mayNominal_bitrat mayMin_bitrat=
  ovError "vorbis_encode_setup_managed" $
    vorbis_encode_setup_managed' vi chanels rat
      (fromMaybe (-1) mayMax_bitrat)
      (fromMaybe (-1) mayNominal_bitrat)
      (fromMaybe (-1) mayMin_bitrat)



-- | Throws 'VorbisError' on negative result.
vorbis_encode_setup_vbr
  :: Ptr VorbisInfo
  -> #{type long}  -- ^ channels
  -> #{type long}  -- ^ rate
  -> #{type float} -- ^ base_quality
  -> IO ()
vorbis_encode_setup_vbr vi chanels rat base_quality =
  ovError "vorbis_encode_setup_vbr" $
    vorbis_encode_setup_vbr' vi chanels rat base_quality
