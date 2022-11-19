{-# LANGUAGE DataKinds
           , DuplicateRecordFields
           , FlexibleInstances
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE TypeApplications #-}

module Vorbisenc.Types where

import           Data.Int
import           Foreign.Storable
import           Foreign.Storable.Offset

#include "vorbis/vorbisenc.h"

data OvectlRatemanageArg =
       OvectlRatemanageArg
         { management_active        :: #type int
         , bitrate_hard_min         :: #type long
         , bitrate_hard_max         :: #type long
         , bitrate_hard_window      :: #type double
         , bitrate_av_lo            :: #type long
         , bitrate_av_hi            :: #type long
         , bitrate_av_window        :: #type double
         , bitrate_av_window_center :: #type double
         }

instance Offset "management_active"        OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, management_active       }
instance Offset "bitrate_hard_min"         OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, bitrate_hard_min        }
instance Offset "bitrate_hard_max"         OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, bitrate_hard_max        }
instance Offset "bitrate_hard_window"      OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, bitrate_hard_window     }
instance Offset "bitrate_av_lo"            OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, bitrate_av_lo           }
instance Offset "bitrate_av_hi"            OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, bitrate_av_hi           }
instance Offset "bitrate_av_window"        OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, bitrate_av_window       }
instance Offset "bitrate_av_window_center" OvectlRatemanageArg where rawOffset = #{offset struct ovectl_ratemanage_arg, bitrate_av_window_center}

instance Storable OvectlRatemanageArg where
  sizeOf _    = #size      struct ovectl_ratemanage_arg
  alignment _ = #alignment struct ovectl_ratemanage_arg

  peek ptr =
    OvectlRatemanageArg
      <$> peek (offset @"management_active"        ptr)
      <*> peek (offset @"bitrate_hard_min"         ptr)
      <*> peek (offset @"bitrate_hard_max"         ptr)
      <*> peek (offset @"bitrate_hard_window"      ptr)
      <*> peek (offset @"bitrate_av_lo"            ptr)
      <*> peek (offset @"bitrate_av_hi"            ptr)
      <*> peek (offset @"bitrate_av_window"        ptr)
      <*> peek (offset @"bitrate_av_window_center" ptr)

  poke ptr val = do
    pokeField @"management_active"        ptr val
    pokeField @"bitrate_hard_min"         ptr val
    pokeField @"bitrate_hard_max"         ptr val
    pokeField @"bitrate_hard_window"      ptr val
    pokeField @"bitrate_av_lo"            ptr val
    pokeField @"bitrate_av_hi"            ptr val
    pokeField @"bitrate_av_window"        ptr val
    pokeField @"bitrate_av_window_center" ptr val



data OvectlRatemanage2Arg =
       OvectlRatemanage2Arg
         { management_active            :: #type int
         , bitrate_limit_min_kbps       :: #type long
         , bitrate_limit_max_kbps       :: #type long
         , bitrate_limit_reservoir_bits :: #type long
         , bitrate_limit_reservoir_bias :: #type double
         , bitrate_average_kbps         :: #type long
         , bitrate_average_damping      :: #type double
         }

instance Offset "management_active"            OvectlRatemanage2Arg where rawOffset = #{offset struct ovectl_ratemanage2_arg, management_active           }
instance Offset "bitrate_limit_min_kbps"       OvectlRatemanage2Arg where rawOffset = #{offset struct ovectl_ratemanage2_arg, bitrate_limit_min_kbps      }
instance Offset "bitrate_limit_max_kbps"       OvectlRatemanage2Arg where rawOffset = #{offset struct ovectl_ratemanage2_arg, bitrate_limit_max_kbps      }
instance Offset "bitrate_limit_reservoir_bits" OvectlRatemanage2Arg where rawOffset = #{offset struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bits}
instance Offset "bitrate_limit_reservoir_bias" OvectlRatemanage2Arg where rawOffset = #{offset struct ovectl_ratemanage2_arg, bitrate_limit_reservoir_bias}
instance Offset "bitrate_average_kbps"         OvectlRatemanage2Arg where rawOffset = #{offset struct ovectl_ratemanage2_arg, bitrate_average_kbps        }
instance Offset "bitrate_average_damping"      OvectlRatemanage2Arg where rawOffset = #{offset struct ovectl_ratemanage2_arg, bitrate_average_damping     }

instance Storable OvectlRatemanage2Arg where
  sizeOf _    = #size      struct ovectl_ratemanage2_arg
  alignment _ = #alignment struct ovectl_ratemanage2_arg

  peek ptr =
    OvectlRatemanage2Arg
      <$> peek (offset @"management_active"            ptr)
      <*> peek (offset @"bitrate_limit_min_kbps"       ptr)
      <*> peek (offset @"bitrate_limit_max_kbps"       ptr)
      <*> peek (offset @"bitrate_limit_reservoir_bits" ptr)
      <*> peek (offset @"bitrate_limit_reservoir_bias" ptr)
      <*> peek (offset @"bitrate_average_kbps"         ptr)
      <*> peek (offset @"bitrate_average_damping"      ptr)

  poke ptr val = do
    pokeField @"management_active"            ptr val
    pokeField @"bitrate_limit_min_kbps"       ptr val
    pokeField @"bitrate_limit_max_kbps"       ptr val
    pokeField @"bitrate_limit_reservoir_bits" ptr val
    pokeField @"bitrate_limit_reservoir_bias" ptr val
    pokeField @"bitrate_average_kbps"         ptr val
    pokeField @"bitrate_average_damping"      ptr val
