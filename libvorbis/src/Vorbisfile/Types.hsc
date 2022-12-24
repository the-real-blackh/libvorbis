{-# LANGUAGE CApiFFI
           , CPP
           , DataKinds
           , DuplicateRecordFields
           , FlexibleInstances
           , ForeignFunctionInterface
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE PatternSynonyms
           , TypeApplications #-}

module Vorbisfile.Types where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset

#include <stdio.h>
#include "vorbis/vorbisfile.h"
#include "extra/vorbisfile.h"

data {-# CTYPE "vorbis/vorbisfile.h" "OggVorbis_File" #-} OggVorbis_File =
       OggVorbis_File
         { datasource       :: Ptr ()
         , seekable         :: #type int
         , offset           :: Ogg_int64_t
         , end              :: Ogg_int64_t
         , oy               :: Ogg_sync_state
         , links            :: #type int
         , offsets          :: Ptr Ogg_int64_t
         , dataoffsets      :: Ptr Ogg_int64_t
         , serialnos        :: Ptr #type long
         , pcmlengths       :: Ptr Ogg_int64_t
         , vi               :: Ptr Vorbis_info
         , vc               :: Ptr Vorbis_comment
         , pcm_offset       :: Ogg_int64_t
         , ready_state      :: #type int
         , current_serialno :: #type long
         , current_link     :: #type long
         , bittrack         :: Ogg_int64_t
         , samptrack        :: Ogg_int64_t
         , os               :: Ogg_stream_state
         , vd               :: Vorbis_dsp_state
         , vb               :: Vorbis_block
         , callbacks        :: Ov_callbackss
         }

instance Offset "datasource"       OggVorbis_File where rawOffset = #{offset OggVorbis_File, datasource      }
instance Offset "seekable"         OggVorbis_File where rawOffset = #{offset OggVorbis_File, seekable        }
instance Offset "offset"           OggVorbis_File where rawOffset = #{offset OggVorbis_File, offset          }
instance Offset "end"              OggVorbis_File where rawOffset = #{offset OggVorbis_File, end             }
instance Offset "oy"               OggVorbis_File where rawOffset = #{offset OggVorbis_File, oy              }
instance Offset "links"            OggVorbis_File where rawOffset = #{offset OggVorbis_File, links           }
instance Offset "offsets"          OggVorbis_File where rawOffset = #{offset OggVorbis_File, offsets         }
instance Offset "dataoffsets"      OggVorbis_File where rawOffset = #{offset OggVorbis_File, dataoffsets     }
instance Offset "serialnos"        OggVorbis_File where rawOffset = #{offset OggVorbis_File, serialnos       }
instance Offset "pcmlengths"       OggVorbis_File where rawOffset = #{offset OggVorbis_File, pcmlengths      }
instance Offset "vi"               OggVorbis_File where rawOffset = #{offset OggVorbis_File, vi              }
instance Offset "vc"               OggVorbis_File where rawOffset = #{offset OggVorbis_File, vc              }
instance Offset "pcm_offset"       OggVorbis_File where rawOffset = #{offset OggVorbis_File, pcm_offset      }
instance Offset "ready_state"      OggVorbis_File where rawOffset = #{offset OggVorbis_File, ready_state     }
instance Offset "current_serialno" OggVorbis_File where rawOffset = #{offset OggVorbis_File, current_serialno}
instance Offset "current_link"     OggVorbis_File where rawOffset = #{offset OggVorbis_File, current_link    }
instance Offset "bittrack"         OggVorbis_File where rawOffset = #{offset OggVorbis_File, bittrack        }
instance Offset "samptrack"        OggVorbis_File where rawOffset = #{offset OggVorbis_File, samptrack       }
instance Offset "os"               OggVorbis_File where rawOffset = #{offset OggVorbis_File, os              }
instance Offset "vd"               OggVorbis_File where rawOffset = #{offset OggVorbis_File, vd              }
instance Offset "vb"               OggVorbis_File where rawOffset = #{offset OggVorbis_File, vb              }
instance Offset "callbacks"        OggVorbis_File where rawOffset = #{offset OggVorbis_File, callbacks       }

instance Storable OggVorbis_File where
  sizeOf _    = #size      OggVorbis_File
  alignment _ = #alignment OggVorbis_File

  peek ptr_ =
    OggVorbis_File
      <$> peek (Foreign.Storable.Offset.offset @"datasource"       ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"seekable"         ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"offset"           ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"end"              ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"oy"               ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"links"            ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"offsets"          ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"dataoffsets"      ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"serialnos"        ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"pcmlengths"       ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"vi"               ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"vc"               ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"pcm_offset"       ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"ready_state"      ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"current_serialno" ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"current_link"     ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"bittrack"         ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"samptrack"        ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"os"               ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"vd"               ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"vb"               ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"callbacks"        ptr_)

  poke ptr_ val = do
    pokeField @"datasource"       ptr_ val
    pokeField @"seekable"         ptr_ val
    pokeField @"offset"           ptr_ val
    pokeField @"end"              ptr_ val
    pokeField @"oy"               ptr_ val
    pokeField @"links"            ptr_ val
    pokeField @"offsets"          ptr_ val
    pokeField @"dataoffsets"      ptr_ val
    pokeField @"serialnos"        ptr_ val
    pokeField @"pcmlengths"       ptr_ val
    pokeField @"vi"               ptr_ val
    pokeField @"vc"               ptr_ val
    pokeField @"pcm_offset"       ptr_ val
    pokeField @"ready_state"      ptr_ val
    pokeField @"current_serialno" ptr_ val
    pokeField @"current_link"     ptr_ val
    pokeField @"bittrack"         ptr_ val
    pokeField @"samptrack"        ptr_ val
    pokeField @"os"               ptr_ val
    pokeField @"vd"               ptr_ val
    pokeField @"vb"               ptr_ val
    pokeField @"callbacks"        ptr_ val



type Read_func =
          Ptr ()            -- ^ @ptr@
       -> #{type size_t}    -- ^ @size@
       -> #{type size_t}    -- ^ @count@
       -> Ptr ()            -- ^ @stream@
       -> IO #{type size_t}

type Seek_func =
          Ptr ()         -- ^ @datasource@
       -> Ogg_int64_t    -- ^ @offset@
       -> #{type int}    -- ^ @origin@
       -> IO #{type int}

type Close_func =
          Ptr ()
       -> IO #{type int}

type Tell_func =
          Ptr ()
       -> IO #{type long}

data {-# CTYPE "vorbis/vorbisfile.h" "ov_callbacks" #-} Ov_callbackss =
       Ov_callbackss
         { read_func  :: FunPtr Read_func
         , seek_func  :: FunPtr Seek_func
         , close_func :: FunPtr Close_func
         , tell_func  :: FunPtr Tell_func
         }

instance Offset "read_func"  Ov_callbackss where rawOffset = #{offset ov_callbacks, read_func  }
instance Offset "seek_func"  Ov_callbackss where rawOffset = #{offset ov_callbacks, seek_func  }
instance Offset "close_func" Ov_callbackss where rawOffset = #{offset ov_callbacks, close_func }
instance Offset "tell_func"  Ov_callbackss where rawOffset = #{offset ov_callbacks, tell_func  }

instance Storable Ov_callbackss where
  sizeOf _    = #size      ov_callbacks
  alignment _ = #alignment ov_callbacks

  peek ptr_ =
    Ov_callbackss
      <$> peek (Foreign.Storable.Offset.offset @"read_func"  ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"seek_func"  ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"close_func" ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"tell_func"  ptr_)

  poke ptr_ val = do
    pokeField @"read_func"  ptr_ val
    pokeField @"seek_func"  ptr_ val
    pokeField @"close_func" ptr_ val
    pokeField @"tell_func"  ptr_ val



foreign import CALLCV unsafe "extra/vorbisfile.h ov_callbacks_default_ptr"
  poke_OV_CALLBACKS_DEFAULT :: Ptr Ov_callbackss -> IO ()

foreign import CALLCV unsafe "extra/vorbisfile.h ov_callbacks_noclose_ptr"
  poke_OV_CALLBACKS_NOCLOSE :: Ptr Ov_callbackss -> IO ()

foreign import CALLCV unsafe "extra/vorbisfile.h ov_callbacks_streamonly_ptr"
  poke_OV_CALLBACKS_STREAMONLY :: Ptr Ov_callbackss -> IO ()

foreign import CALLCV unsafe "extra/vorbisfile.h ov_callbacks_streamonly_noclose_ptr"
  poke_OV_CALLBACKS_STREAMONLY_NOCLOSE :: Ptr Ov_callbackss -> IO ()
