module Codec.Audio.Vorbis.Enc.Raw.Types where

import           Data.Function ((&))
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

instance Storable OvectlRatemanageArg where
  sizeOf _    = #size      struct ovectl_ratemanage_arg
  alignment _ = #alignment struct ovectl_ratemanage_arg

  peek ptr =
    OvectlRatemanageArg
      <$> #{peek struct ovectl_ratemanage_arg, management_active       } ptr
      <*> #{peek struct ovectl_ratemanage_arg, bitrate_hard_min        } ptr
      <*> #{peek struct ovectl_ratemanage_arg, bitrate_hard_max        } ptr
      <*> #{peek struct ovectl_ratemanage_arg, bitrate_hard_window     } ptr
      <*> #{peek struct ovectl_ratemanage_arg, bitrate_av_lo           } ptr
      <*> #{peek struct ovectl_ratemanage_arg, bitrate_av_hi           } ptr
      <*> #{peek struct ovectl_ratemanage_arg, bitrate_av_window       } ptr
      <*> #{peek struct ovectl_ratemanage_arg, bitrate_av_window_center} ptr

  poke ptr val = do
    #{poke struct ovectl_ratemanage_arg, management_active       } ptr $ val & oraManagement_active
    #{poke struct ovectl_ratemanage_arg, bitrate_hard_min        } ptr $ val & oraBitrate_hard_min
    #{poke struct ovectl_ratemanage_arg, bitrate_hard_max        } ptr $ val & oraBitrate_hard_max
    #{poke struct ovectl_ratemanage_arg, bitrate_hard_window     } ptr $ val & oraBitrate_hard_window
    #{poke struct ovectl_ratemanage_arg, bitrate_av_lo           } ptr $ val & oraBitrate_av_lo
    #{poke struct ovectl_ratemanage_arg, bitrate_av_hi           } ptr $ val & oraBitrate_av_hi
    #{poke struct ovectl_ratemanage_arg, bitrate_av_window       } ptr $ val & oraBitrate_av_window
    #{poke struct ovectl_ratemanage_arg, bitrate_av_window_center} ptr $ val & oraBitrate_av_window_center



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

instance Storable OvectlRatemanage2Arg where
  sizeOf _    = #size      struct ovectl_ratemanage2_arg
  alignment _ = #alignment struct ovectl_ratemanage2_arg

  peek ptr =
    OvectlRatemanage2Arg
      <$> #{peek struct ovectl_ratemanage2_arg, management_active           } ptr
      <*> #{peek struct ovectl_ratemanage2_arg, bitrate_limit_min_kbps      } ptr
      <*> #{peek struct ovectl_ratemanage2_arg, bitrate_limit_max_kbps      } ptr
      <*> #{peek struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bits} ptr
      <*> #{peek struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bias} ptr
      <*> #{peek struct ovectl_ratemanage2_arg, bitrate_average_kbps        } ptr
      <*> #{peek struct ovectl_ratemanage2_arg, bitrate_average_damping     } ptr

  poke ptr val = do
    #{poke struct ovectl_ratemanage2_arg, management_active           } ptr $ val & or2aManagement_active
    #{poke struct ovectl_ratemanage2_arg, bitrate_limit_min_kbps      } ptr $ val & or2aBitrate_limit_min_kbps
    #{poke struct ovectl_ratemanage2_arg, bitrate_limit_max_kbps      } ptr $ val & or2aBitrate_limit_max_kbps
    #{poke struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bits} ptr $ val & or2aBitrate_limit_reservoir_bits
    #{poke struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bias} ptr $ val & or2aBitrate_limit_reservoir_bias
    #{poke struct ovectl_ratemanage2_arg, bitrate_average_kbps        } ptr $ val & or2aBitrate_average_kbps
    #{poke struct ovectl_ratemanage2_arg, bitrate_average_damping     } ptr $ val & or2aBitrate_average_damping
