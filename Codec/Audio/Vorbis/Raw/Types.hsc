{-# LANGUAGE DataKinds
           , EmptyDataDecls
           , FlexibleInstances
           , ForeignFunctionInterface
           , TemplateHaskell
           , TypeApplications #-}

module Codec.Audio.Vorbis.Raw.Types where

import           Data.Field
import           Data.Field.Storable.TH

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
         { vcUser_comments   :: Ptr (Ptr #{type char})
         , vcComment_lengths :: Ptr #type int
         , vcComments        :: #type int
         , vcVendor          :: Ptr #type char
         }
       deriving Show

deriveStorable #{offset struct vorbis_comment, user_comments  } "vcUser_comments"   ''VorbisComment
deriveStorable #{offset struct vorbis_comment, comment_lengths} "vcComment_lengths" ''VorbisComment
deriveStorable #{offset struct vorbis_comment, comments       } "vcComments"        ''VorbisComment
deriveStorable #{offset struct vorbis_comment, vendor         } "vcVendor"          ''VorbisComment

instance Storable VorbisComment where
  sizeOf _    = #size      struct vorbis_comment
  alignment _ = #alignment struct vorbis_comment

  peek ptr =
    VorbisComment
      <$> peekField @"vcUser_comments"   ptr
      <*> peekField @"vcComment_lengths" ptr
      <*> peekField @"vcComments"        ptr
      <*> peekField @"vcVendor"          ptr

  poke ptr val = do
    pokeRecordField @"vcUser_comments"   ptr val
    pokeRecordField @"vcComment_lengths" ptr val
    pokeRecordField @"vcComments"        ptr val
    pokeRecordField @"vcVendor"          ptr val



data VorbisDspState = VorbisDspState
                      deriving Show

instance Storable VorbisDspState where
  sizeOf _    = #size      struct vorbis_dsp_state
  alignment _ = #alignment struct vorbis_dsp_state

  peek _ = return VorbisDspState

  poke _ _ = return ()



data VorbisInfo =
       VorbisInfo
         { viVersion         :: #type int
         , viChannels        :: #type int
         , viRate            :: #type long
         , viBitrate_upper   :: #type long
         , viBitrate_nominal :: #type long
         , viBitrate_lower   :: #type long
         , viBitrate_window  :: #type long
         , viCodec_setup     :: Ptr ()
         }
       deriving Show

deriveStorable #{offset struct vorbis_info, version        } "viVersion"         ''VorbisInfo
deriveStorable #{offset struct vorbis_info, channels       } "viChannels"        ''VorbisInfo
deriveStorable #{offset struct vorbis_info, rate           } "viRate"            ''VorbisInfo
deriveStorable #{offset struct vorbis_info, bitrate_upper  } "viBitrate_upper"   ''VorbisInfo
deriveStorable #{offset struct vorbis_info, bitrate_nominal} "viBitrate_nominal" ''VorbisInfo
deriveStorable #{offset struct vorbis_info, bitrate_lower  } "viBitrate_lower"   ''VorbisInfo
deriveStorable #{offset struct vorbis_info, bitrate_window } "viBitrate_window"  ''VorbisInfo
deriveStorable #{offset struct vorbis_info, codec_setup    } "viCodec_setup"     ''VorbisInfo

instance Storable VorbisInfo where
  sizeOf _    = #size      struct vorbis_info
  alignment _ = #alignment struct vorbis_info

  peek ptr =
    VorbisInfo
      <$> peekField @"viVersion"         ptr
      <*> peekField @"viChannels"        ptr
      <*> peekField @"viRate"            ptr
      <*> peekField @"viBitrate_upper"   ptr
      <*> peekField @"viBitrate_nominal" ptr
      <*> peekField @"viBitrate_lower"   ptr
      <*> peekField @"viBitrate_window"  ptr
      <*> peekField @"viCodec_setup"     ptr

  poke ptr val = do
    pokeRecordField @"viVersion"         ptr val
    pokeRecordField @"viChannels"        ptr val
    pokeRecordField @"viRate"            ptr val
    pokeRecordField @"viBitrate_upper"   ptr val
    pokeRecordField @"viBitrate_nominal" ptr val
    pokeRecordField @"viBitrate_lower"   ptr val
    pokeRecordField @"viBitrate_window"  ptr val
    pokeRecordField @"viCodec_setup"     ptr val
