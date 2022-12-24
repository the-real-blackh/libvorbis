{-# LANGUAGE DataKinds
           , DuplicateRecordFields
           , EmptyDataDecls
           , FlexibleInstances
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE TypeApplications #-}

module Libvorbis.Types where

import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset

#include "vorbis/codec.h"

data {-# CTYPE "vorbis/codec.h" "vorbis_block" #-} Vorbis_block = Vorbis_block

instance Storable Vorbis_block where
  sizeOf    _ = #size      struct vorbis_block
  alignment _ = #alignment struct vorbis_block

  peek _ = return Vorbis_block

  poke _ _ = return ()



data {-# CTYPE "vorbis/codec.h" "vorbis_comment" #-} Vorbis_comment =
       Vorbis_comment
         { user_comments   :: Ptr (Ptr #{type char})
         , comment_lengths :: Ptr #type int
         , comments        :: #type int
         , vendor          :: Ptr #type char
         }

instance Offset "user_comments"   Vorbis_comment where rawOffset = #{offset struct vorbis_comment, user_comments  }
instance Offset "comment_lengths" Vorbis_comment where rawOffset = #{offset struct vorbis_comment, comment_lengths}
instance Offset "comments"        Vorbis_comment where rawOffset = #{offset struct vorbis_comment, comments       }
instance Offset "vendor"          Vorbis_comment where rawOffset = #{offset struct vorbis_comment, vendor         }

instance Storable Vorbis_comment where
  sizeOf _    = #size      struct vorbis_comment
  alignment _ = #alignment struct vorbis_comment

  peek ptr =
    Vorbis_comment
      <$> peek (offset @"user_comments"   ptr)
      <*> peek (offset @"comment_lengths" ptr)
      <*> peek (offset @"comments"        ptr)
      <*> peek (offset @"vendor"          ptr)

  poke ptr val = do
    pokeField @"user_comments"   ptr val
    pokeField @"comment_lengths" ptr val
    pokeField @"comments"        ptr val
    pokeField @"vendor"          ptr val



data {-# CTYPE "vorbis/codec.h" "vorbis_dsp_state" #-} Vorbis_dsp_state = Vorbis_dsp_state

instance Storable Vorbis_dsp_state where
  sizeOf    _ = #size      struct vorbis_dsp_state
  alignment _ = #alignment struct vorbis_dsp_state

  peek _ = return Vorbis_dsp_state

  poke _ _ = return ()



data {-# CTYPE "vorbis/codec.h" "vorbis_info" #-} Vorbis_info =
       Vorbis_info
         { version         :: #type int
         , channels        :: #type int
         , rate            :: #type long
         , bitrate_upper   :: #type long
         , bitrate_nominal :: #type long
         , bitrate_lower   :: #type long
         , bitrate_window  :: #type long
         , codec_setup     :: Ptr ()
         }

instance Offset "version"         Vorbis_info where rawOffset = #{offset struct vorbis_info, version        }
instance Offset "channels"        Vorbis_info where rawOffset = #{offset struct vorbis_info, channels       }
instance Offset "rate"            Vorbis_info where rawOffset = #{offset struct vorbis_info, rate           }
instance Offset "bitrate_upper"   Vorbis_info where rawOffset = #{offset struct vorbis_info, bitrate_upper  }
instance Offset "bitrate_nominal" Vorbis_info where rawOffset = #{offset struct vorbis_info, bitrate_nominal}
instance Offset "bitrate_lower"   Vorbis_info where rawOffset = #{offset struct vorbis_info, bitrate_lower  }
instance Offset "bitrate_window"  Vorbis_info where rawOffset = #{offset struct vorbis_info, bitrate_window }
instance Offset "codec_setup"     Vorbis_info where rawOffset = #{offset struct vorbis_info, codec_setup    }

instance Storable Vorbis_info where
  sizeOf _    = #size      struct vorbis_info
  alignment _ = #alignment struct vorbis_info

  peek ptr =
    Vorbis_info
      <$> peek (offset @"version"         ptr)
      <*> peek (offset @"channels"        ptr)
      <*> peek (offset @"rate"            ptr)
      <*> peek (offset @"bitrate_upper"   ptr)
      <*> peek (offset @"bitrate_nominal" ptr)
      <*> peek (offset @"bitrate_lower"   ptr)
      <*> peek (offset @"bitrate_window"  ptr)
      <*> peek (offset @"codec_setup"     ptr)

  poke ptr val = do
    pokeField @"version"         ptr val
    pokeField @"channels"        ptr val
    pokeField @"rate"            ptr val
    pokeField @"bitrate_upper"   ptr val
    pokeField @"bitrate_nominal" ptr val
    pokeField @"bitrate_lower"   ptr val
    pokeField @"bitrate_window"  ptr val
    pokeField @"codec_setup"     ptr val
