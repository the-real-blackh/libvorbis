{-# LANGUAGE DataKinds
           , FlexibleInstances
           , TemplateHaskell
           , TypeApplications #-}

module Codec.Audio.Vorbis.Enc.Raw.Types where

import           Data.Field
import           Data.Field.Storable.TH

import           Data.Int
import           Foreign.Storable

#include "vorbis/vorbisenc.h"

data OvectlRatemanageArg =
       OvectlRatemanageArg
         { oraManagement_active        :: #type int
         , oraBitrate_hard_min         :: #type long
         , oraBitrate_hard_max         :: #type long
         , oraBitrate_hard_window      :: #type double
         , oraBitrate_av_lo            :: #type long
         , oraBitrate_av_hi            :: #type long
         , oraBitrate_av_window        :: #type double
         , oraBitrate_av_window_center :: #type double
         }
       deriving Show

deriveStorable #{offset struct ovectl_ratemanage_arg, management_active        } "oraManagement_active"        ''OvectlRatemanageArg
deriveStorable #{offset struct ovectl_ratemanage_arg, bitrate_hard_min         } "oraBitrate_hard_min"         ''OvectlRatemanageArg
deriveStorable #{offset struct ovectl_ratemanage_arg, bitrate_hard_max         } "oraBitrate_hard_max"         ''OvectlRatemanageArg
deriveStorable #{offset struct ovectl_ratemanage_arg, bitrate_hard_window      } "oraBitrate_hard_window"      ''OvectlRatemanageArg
deriveStorable #{offset struct ovectl_ratemanage_arg, bitrate_av_lo            } "oraBitrate_av_lo"            ''OvectlRatemanageArg
deriveStorable #{offset struct ovectl_ratemanage_arg, bitrate_av_hi            } "oraBitrate_av_hi"            ''OvectlRatemanageArg
deriveStorable #{offset struct ovectl_ratemanage_arg, bitrate_av_window        } "oraBitrate_av_window"        ''OvectlRatemanageArg
deriveStorable #{offset struct ovectl_ratemanage_arg, bitrate_av_window_center } "oraBitrate_av_window_center" ''OvectlRatemanageArg

instance Storable OvectlRatemanageArg where
  sizeOf _    = #size      struct ovectl_ratemanage_arg
  alignment _ = #alignment struct ovectl_ratemanage_arg

  peek ptr =
    OvectlRatemanageArg
      <$> peekField @"oraManagement_active"        ptr
      <*> peekField @"oraBitrate_hard_min"         ptr
      <*> peekField @"oraBitrate_hard_max"         ptr
      <*> peekField @"oraBitrate_hard_window"      ptr
      <*> peekField @"oraBitrate_av_lo"            ptr
      <*> peekField @"oraBitrate_av_hi"            ptr
      <*> peekField @"oraBitrate_av_window"        ptr
      <*> peekField @"oraBitrate_av_window_center" ptr

  poke ptr val = do
    pokeRecordField @"oraManagement_active"        ptr val
    pokeRecordField @"oraBitrate_hard_min"         ptr val
    pokeRecordField @"oraBitrate_hard_max"         ptr val
    pokeRecordField @"oraBitrate_hard_window"      ptr val
    pokeRecordField @"oraBitrate_av_lo"            ptr val
    pokeRecordField @"oraBitrate_av_hi"            ptr val
    pokeRecordField @"oraBitrate_av_window"        ptr val
    pokeRecordField @"oraBitrate_av_window_center" ptr val



data OvectlRatemanage2Arg =
       OvectlRatemanage2Arg
         { or2aManagement_active            :: #type int
         , or2aBitrate_limit_min_kbps       :: #type long
         , or2aBitrate_limit_max_kbps       :: #type long
         , or2aBitrate_limit_reservoir_bits :: #type long
         , or2aBitrate_limit_reservoir_bias :: #type double
         , or2aBitrate_average_kbps         :: #type long
         , or2aBitrate_average_damping      :: #type double
         }
       deriving Show

deriveStorable #{offset struct ovectl_ratemanage2_arg, management_active            } "or2aManagement_active"            ''OvectlRatemanage2Arg
deriveStorable #{offset struct ovectl_ratemanage2_arg, bitrate_limit_min_kbps       } "or2aBitrate_limit_min_kbps"       ''OvectlRatemanage2Arg
deriveStorable #{offset struct ovectl_ratemanage2_arg, bitrate_limit_max_kbps       } "or2aBitrate_limit_max_kbps"       ''OvectlRatemanage2Arg
deriveStorable #{offset struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bits } "or2aBitrate_limit_reservoir_bits" ''OvectlRatemanage2Arg
deriveStorable #{offset struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bias } "or2aBitrate_limit_reservoir_bias" ''OvectlRatemanage2Arg
deriveStorable #{offset struct ovectl_ratemanage2_arg, bitrate_average_kbps         } "or2aBitrate_average_kbps"         ''OvectlRatemanage2Arg
deriveStorable #{offset struct ovectl_ratemanage2_arg, bitrate_average_damping      } "or2aBitrate_average_damping"      ''OvectlRatemanage2Arg

instance Storable OvectlRatemanage2Arg where
  sizeOf _    = #size      struct ovectl_ratemanage2_arg
  alignment _ = #alignment struct ovectl_ratemanage2_arg

  peek ptr =
    OvectlRatemanage2Arg
      <$> peekField @"or2aManagement_active"            ptr
      <*> peekField @"or2aBitrate_limit_min_kbps"       ptr
      <*> peekField @"or2aBitrate_limit_max_kbps"       ptr
      <*> peekField @"or2aBitrate_limit_reservoir_bits" ptr
      <*> peekField @"or2aBitrate_limit_reservoir_bias" ptr
      <*> peekField @"or2aBitrate_average_kbps"         ptr
      <*> peekField @"or2aBitrate_average_damping"      ptr

  poke ptr val = do
    pokeRecordField @"or2aManagement_active"            ptr val
    pokeRecordField @"or2aBitrate_limit_min_kbps"       ptr val
    pokeRecordField @"or2aBitrate_limit_max_kbps"       ptr val
    pokeRecordField @"or2aBitrate_limit_reservoir_bits" ptr val
    pokeRecordField @"or2aBitrate_limit_reservoir_bias" ptr val
    pokeRecordField @"or2aBitrate_average_kbps"         ptr val
    pokeRecordField @"or2aBitrate_average_damping"      ptr val
