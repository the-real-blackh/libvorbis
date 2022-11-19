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

data VorbisBlock = VorbisBlock

instance Storable VorbisBlock where
  sizeOf    _ = #size      struct vorbis_block
  alignment _ = #alignment struct vorbis_block

  peek _ = return VorbisBlock

  poke _ _ = return ()



data VorbisComment =
       VorbisComment
         { user_comments   :: Ptr (Ptr #{type char})
         , comment_lengths :: Ptr #type int
         , comments        :: #type int
         , vendor          :: Ptr #type char
         }

instance Offset "user_comments"   VorbisComment where rawOffset = #{offset struct vorbis_comment, user_comments  }
instance Offset "comment_lengths" VorbisComment where rawOffset = #{offset struct vorbis_comment, comment_lengths}
instance Offset "comments"        VorbisComment where rawOffset = #{offset struct vorbis_comment, comments       }
instance Offset "vendor"          VorbisComment where rawOffset = #{offset struct vorbis_comment, vendor         }

instance Storable VorbisComment where
  sizeOf _    = #size      struct vorbis_comment
  alignment _ = #alignment struct vorbis_comment

  peek ptr =
    VorbisComment
      <$> peek (offset @"user_comments"   ptr)
      <*> peek (offset @"comment_lengths" ptr)
      <*> peek (offset @"comments"        ptr)
      <*> peek (offset @"vendor"          ptr)

  poke ptr val = do
    pokeField @"user_comments"   ptr val
    pokeField @"comment_lengths" ptr val
    pokeField @"comments"        ptr val
    pokeField @"vendor"          ptr val



data VorbisDspState = VorbisDspState

instance Storable VorbisDspState where
  sizeOf    _ = #size      struct vorbis_dsp_state
  alignment _ = #alignment struct vorbis_dsp_state

  peek _ = return VorbisDspState

  poke _ _ = return ()



data VorbisInfo =
       VorbisInfo
         { version         :: #type int
         , channels        :: #type int
         , rate            :: #type long
         , bitrate_upper   :: #type long
         , bitrate_nominal :: #type long
         , bitrate_lower   :: #type long
         , bitrate_window  :: #type long
         , codec_setup     :: Ptr ()
         }

instance Offset "version"         VorbisInfo where rawOffset = #{offset struct vorbis_info, version        }
instance Offset "channels"        VorbisInfo where rawOffset = #{offset struct vorbis_info, channels       }
instance Offset "rate"            VorbisInfo where rawOffset = #{offset struct vorbis_info, rate           }
instance Offset "bitrate_upper"   VorbisInfo where rawOffset = #{offset struct vorbis_info, bitrate_upper  }
instance Offset "bitrate_nominal" VorbisInfo where rawOffset = #{offset struct vorbis_info, bitrate_nominal}
instance Offset "bitrate_lower"   VorbisInfo where rawOffset = #{offset struct vorbis_info, bitrate_lower  }
instance Offset "bitrate_window"  VorbisInfo where rawOffset = #{offset struct vorbis_info, bitrate_window }
instance Offset "codec_setup"     VorbisInfo where rawOffset = #{offset struct vorbis_info, codec_setup    }

instance Storable VorbisInfo where
  sizeOf _    = #size      struct vorbis_info
  alignment _ = #alignment struct vorbis_info

  peek ptr =
    VorbisInfo
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
