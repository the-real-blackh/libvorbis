{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Types where

import           Data.Function ((&))
import           Foreign

#include "vorbis/codec.h"

data VorbisBlock = VorbisBlock
                   deriving Show

instance Storable VorbisBlock where
  sizeOf _    = #size      struct vorbis_block
  alignment _ = #alignment struct vorbis_block

  peek _ = return VorbisBlock

  poke _ _ = return ()



data VorbisComment =
       VorbisComment
         { userComments   :: Ptr (Ptr #{type char})
         , commentLengths :: Ptr #type int
         , comments       :: #type int
         , vendor         :: Ptr #type char
         }
       deriving Show

instance Storable VorbisComment where
  sizeOf _    = #size      struct vorbis_comment
  alignment _ = #alignment struct vorbis_comment

  peek ptr =
    VorbisComment
      <$> #{peek struct vorbis_comment, user_comments  } ptr
      <*> #{peek struct vorbis_comment, comment_lengths} ptr
      <*> #{peek struct vorbis_comment, comments       } ptr
      <*> #{peek struct vorbis_comment, vendor         } ptr

  poke ptr val = do
    #{poke struct vorbis_comment, user_comments  } ptr $ val & userComments
    #{poke struct vorbis_comment, comment_lengths} ptr $ val & commentLengths
    #{poke struct vorbis_comment, comments       } ptr $ val & comments
    #{poke struct vorbis_comment, vendor         } ptr $ val & vendor



data VorbisDspState = VorbisDspState
                      deriving Show

instance Storable VorbisDspState where
  sizeOf _    = #size      struct vorbis_dsp_state
  alignment _ = #alignment struct vorbis_dsp_state

  peek _ = return VorbisDspState

  poke _ _ = return ()



data VorbisInfo =
       VorbisInfo
         { version        :: #type int
         , channels       :: #type int
         , rate           :: #type long
         , bitrateUpper   :: #type long
         , bitrateNominal :: #type long
         , bitrateLower   :: #type long
         , bitrateWindow  :: #type long
         , codecSetup     :: Ptr ()
         }
       deriving Show

instance Storable VorbisInfo where
  sizeOf _    = #size      struct vorbis_info
  alignment _ = #alignment struct vorbis_info

  peek ptr =
    VorbisInfo
      <$> #{peek struct vorbis_info, version        } ptr
      <*> #{peek struct vorbis_info, channels       } ptr
      <*> #{peek struct vorbis_info, rate           } ptr
      <*> #{peek struct vorbis_info, bitrate_upper  } ptr
      <*> #{peek struct vorbis_info, bitrate_nominal} ptr
      <*> #{peek struct vorbis_info, bitrate_lower  } ptr
      <*> #{peek struct vorbis_info, bitrate_window } ptr
      <*> #{peek struct vorbis_info, codec_setup    } ptr

  poke ptr val = do
    #{poke struct vorbis_info, version        } ptr $ val & version
    #{poke struct vorbis_info, channels       } ptr $ val & channels
    #{poke struct vorbis_info, rate           } ptr $ val & rate
    #{poke struct vorbis_info, bitrate_upper  } ptr $ val & bitrateUpper
    #{poke struct vorbis_info, bitrate_nominal} ptr $ val & bitrateNominal
    #{poke struct vorbis_info, bitrate_lower  } ptr $ val & bitrateLower
    #{poke struct vorbis_info, bitrate_window } ptr $ val & bitrateWindow
    #{poke struct vorbis_info, codec_setup    } ptr $ val & codecSetup
