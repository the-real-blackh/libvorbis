{-# LANGUAGE DataKinds
           , DuplicateRecordFields
           , FlexibleInstances
           , ForeignFunctionInterface
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE PatternSynonyms
           , TypeApplications #-}

module Vorbisfile.Types
{-( OggVorbisFile (..)
  , ReadFunc
  , SeekFunc
  , CloseFunc
  , TellFunc
  , OvCallbacks (..)
  , ovCallbacks
  , pattern OV_CALLBACKS_DEFAULT
  , pattern OV_CALLBACKS_NOCLOSE
  , pattern OV_CALLBACKS_STREAMONLY
  , pattern OV_CALLBACKS_STREAMONLY_NOCLOSE
  , OvMemoryPointer
  , ovMemoryPointer
  , pattern OV_CALLBACKS_FROM_MEMORY
  )-} where

import           Libogg
import           Libvorbis.Types

import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset

#include <stdio.h>
#include "vorbis/vorbisfile.h"
#include "vorbis/vorbisfile-plus.h"

data OggVorbisFile =
       OggVorbisFile
         { datasource       :: Ptr ()
         , seekable         :: #type int
         , offset           :: Ogg_Int64_t
         , end              :: Ogg_Int64_t
         , oy               :: OggSyncState
         , links            :: #type int
         , offsets          :: Ptr Ogg_Int64_t
         , dataoffsets      :: Ptr Ogg_Int64_t
         , serialnos        :: Ptr #type long
         , pcmlengths       :: Ptr Ogg_Int64_t
         , vi               :: Ptr VorbisInfo
         , vc               :: Ptr VorbisComment
         , pcm_offset       :: Ogg_Int64_t
         , ready_state      :: #type int
         , current_serialno :: #type long
         , current_link     :: #type long
         , bittrack         :: Ogg_Int64_t
         , samptrack        :: Ogg_Int64_t
         , os               :: OggStreamState
         , vd               :: VorbisDspState
         , vb               :: VorbisBlock
         , callbacks        :: OvCallbacks
         }

instance Offset "datasource"       OggVorbisFile where rawOffset = #{offset OggVorbis_File, datasource      }
instance Offset "seekable"         OggVorbisFile where rawOffset = #{offset OggVorbis_File, seekable        }
instance Offset "offset"           OggVorbisFile where rawOffset = #{offset OggVorbis_File, offset          }
instance Offset "end"              OggVorbisFile where rawOffset = #{offset OggVorbis_File, end             }
instance Offset "oy"               OggVorbisFile where rawOffset = #{offset OggVorbis_File, oy              }
instance Offset "links"            OggVorbisFile where rawOffset = #{offset OggVorbis_File, links           }
instance Offset "offsets"          OggVorbisFile where rawOffset = #{offset OggVorbis_File, offsets         }
instance Offset "dataoffsets"      OggVorbisFile where rawOffset = #{offset OggVorbis_File, dataoffsets     }
instance Offset "serialnos"        OggVorbisFile where rawOffset = #{offset OggVorbis_File, serialnos       }
instance Offset "pcmlengths"       OggVorbisFile where rawOffset = #{offset OggVorbis_File, pcmlengths      }
instance Offset "vi"               OggVorbisFile where rawOffset = #{offset OggVorbis_File, vi              }
instance Offset "vc"               OggVorbisFile where rawOffset = #{offset OggVorbis_File, vc              }
instance Offset "pcm_offset"       OggVorbisFile where rawOffset = #{offset OggVorbis_File, pcm_offset      }
instance Offset "ready_state"      OggVorbisFile where rawOffset = #{offset OggVorbis_File, ready_state     }
instance Offset "current_serialno" OggVorbisFile where rawOffset = #{offset OggVorbis_File, current_serialno}
instance Offset "current_link"     OggVorbisFile where rawOffset = #{offset OggVorbis_File, current_link    }
instance Offset "bittrack"         OggVorbisFile where rawOffset = #{offset OggVorbis_File, bittrack        }
instance Offset "samptrack"        OggVorbisFile where rawOffset = #{offset OggVorbis_File, samptrack       }
instance Offset "os"               OggVorbisFile where rawOffset = #{offset OggVorbis_File, os              }
instance Offset "vd"               OggVorbisFile where rawOffset = #{offset OggVorbis_File, vd              }
instance Offset "vb"               OggVorbisFile where rawOffset = #{offset OggVorbis_File, vb              }
instance Offset "callbacks"        OggVorbisFile where rawOffset = #{offset OggVorbis_File, callbacks       }

instance Storable OggVorbisFile where
  sizeOf _    = #size      OggVorbis_File
  alignment _ = #alignment OggVorbis_File

  peek ptr_ =
    OggVorbisFile
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



type ReadFunc = Ptr ()            -- ^ @ptr@
             -> #{type size_t}    -- ^ @size@
             -> #{type size_t}    -- ^ @count@
             -> Ptr ()            -- ^ @stream@
             -> IO #{type size_t}

type SeekFunc  = Ptr ()         -- ^ @datasource@
              -> Ogg_Int64_t    -- ^ @offset@
              -> #{type int}    -- ^ @origin@
              -> IO #{type int}

type CloseFunc = Ptr ()
              -> IO #{type int}

type TellFunc = Ptr ()
             -> IO #{type long}

data OvCallbacks =
       OvCallbacks
         { read_func  :: FunPtr ReadFunc
         , seek_func  :: FunPtr SeekFunc
         , close_func :: FunPtr CloseFunc
         , tell_func  :: FunPtr TellFunc
         }

instance Offset "read_func"  OvCallbacks where rawOffset = #{offset ov_callbacks, read_func  }
instance Offset "seek_func"  OvCallbacks where rawOffset = #{offset ov_callbacks, seek_func  }
instance Offset "close_func" OvCallbacks where rawOffset = #{offset ov_callbacks, close_func }
instance Offset "tell_func"  OvCallbacks where rawOffset = #{offset ov_callbacks, tell_func  }

instance Storable OvCallbacks where
  sizeOf _    = #size      ov_callbacks
  alignment _ = #alignment ov_callbacks

  peek ptr_ =
    OvCallbacks
      <$> peek (Foreign.Storable.Offset.offset @"read_func"  ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"seek_func"  ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"close_func" ptr_)
      <*> peek (Foreign.Storable.Offset.offset @"tell_func"  ptr_)

  poke ptr_ val = do
    pokeField @"read_func"  ptr_ val
    pokeField @"seek_func"  ptr_ val
    pokeField @"close_func" ptr_ val
    pokeField @"tell_func"  ptr_ val
{-
-- | Converts a set of Haskell functions to 'OvCallbacks'.
ovCallbacks
  :: ReadFunc a
  -> Maybe (SeekFunc a)
  -> Maybe (CloseFunc a)
  -> Maybe (TellFunc a)
  -> OvCallbacks a
ovCallbacks raed maySeek mayClose mayTell =
  unsafePerformIO . coerce $
    OvCallbacks
      <$> mkReadFunc  (coerce raed)
      <*> maybe (pure nullFunPtr) (mkSeekFunc  . coerce) maySeek
      <*> maybe (pure nullFunPtr) (mkCloseFunc . coerce) mayClose
      <*> maybe (pure nullFunPtr) (mkTellFunc  . coerce) mayTell



pattern OV_CALLBACKS_DEFAULT :: OvCallbacks ()
pattern OV_CALLBACKS_DEFAULT <- _
  where
    OV_CALLBACKS_DEFAULT =
      unsafePerformIO $
        alloca $ \ptr -> do
          ov_callbacks_default_ptr ptr
          peek ptr

pattern OV_CALLBACKS_NOCLOSE :: OvCallbacks ()
pattern OV_CALLBACKS_NOCLOSE <- _
  where
    OV_CALLBACKS_NOCLOSE =
      unsafePerformIO $
        alloca $ \ptr -> do
          ov_callbacks_noclose_ptr ptr
          peek ptr

pattern OV_CALLBACKS_STREAMONLY :: OvCallbacks ()
pattern OV_CALLBACKS_STREAMONLY <- _
  where
    OV_CALLBACKS_STREAMONLY =
      unsafePerformIO $
        alloca $ \ptr -> do
          ov_callbacks_streamonly_ptr ptr
          peek ptr

pattern OV_CALLBACKS_STREAMONLY_NOCLOSE :: OvCallbacks ()
pattern OV_CALLBACKS_STREAMONLY_NOCLOSE <- _
  where
    OV_CALLBACKS_STREAMONLY_NOCLOSE =
      unsafePerformIO $
        alloca $ \ptr -> do
          ov_callbacks_streamonly_noclose_ptr ptr
          peek ptr



-- | Smart constructor for 'OvMemoryPointer'.
ovMemoryPointer :: Ptr a -> #{type size_t} -> OvMemoryPointer a
ovMemoryPointer datasource total = OvMemoryPointer datasource total 0

-- | Extra 'OvCallbacks' for reading files that reside in memory.
pattern OV_CALLBACKS_FROM_MEMORY :: OvCallbacks (OvMemoryPointer a)
pattern OV_CALLBACKS_FROM_MEMORY <- _
  where
    OV_CALLBACKS_FROM_MEMORY =
      let readFunc buffer size nmemb ptr = do
            mem@(OvMemoryPointer datasource total current) <- peek ptr
            case () of
              () | current >= total               -> return 0
                 | size * nmemb > total - current -> do
                     copyBytes buffer (datasource `plusPtr` fromIntegral current) (fromIntegral $ total - current)
                     poke ptr $ mem { ompCurrent = total }
                     return (total - current)
                 | otherwise                -> do
                     copyBytes buffer (datasource `plusPtr` fromIntegral current) (fromIntegral $ size * nmemb)
                     poke ptr $ mem { ompCurrent = current + size * nmemb }
                     return (size * nmemb)

          seekFunc ptr off origin = do
            mem@(OvMemoryPointer _ total current) <- peek ptr
            let mayNewCurrent =
                  case origin of
                    #{const SEEK_SET} -> Just $ fromIntegral off
                    #{const SEEK_CUR} -> Just $ fromIntegral off + fromIntegral current
                    #{const SEEK_END} -> Just $ fromIntegral off + total
                    _                 -> Nothing
            case mayNewCurrent of
              Nothing         -> return $ -1
              Just newCurrent -> if newCurrent < 0 || newCurrent > total
                                   then return $ -1
                                   else do
                                     poke ptr $ mem { ompCurrent = newCurrent }
                                     return 0
    
          tellFunc ptr = fromIntegral . ompCurrent <$> peek ptr

      in ovCallbacks
           readFunc
           ( Just seekFunc )
           Nothing
           ( Just tellFunc )
           -}