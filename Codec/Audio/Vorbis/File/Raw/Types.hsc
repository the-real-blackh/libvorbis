{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module Codec.Audio.Vorbis.File.Raw.Types
  ( OggVorbisFile (..)
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
  ) where

import           Codec.Audio.Vorbis.File.Raw.Types.Internal
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Coerce
import           Data.Function ((&))
import           Foreign
import           System.IO.Unsafe

#include <stdio.h>
#include "vorbis/vorbisfile.h"
#include "vorbis/vorbisfile-plus.h"

data OggVorbisFile a =
       OggVorbisFile
         { ovfDatasource       :: Ptr a
         , ovfSeekable         :: #type int
         , ovfOffset           :: Ogg_Int64_t
         , ovfEnd              :: Ogg_Int64_t
         , ovfOy               :: OggSyncState
         , ovfLinks            :: #type int
         , ovfOffsets          :: Ptr Ogg_Int64_t
         , ovfDataoffsets      :: Ptr Ogg_Int64_t
         , ovfSerialnos        :: Ptr #type long
         , ovfPcmlengths       :: Ptr Ogg_Int64_t
         , ovfVi               :: Ptr VorbisInfo
         , ovfVc               :: Ptr VorbisComment
         , ovfPcm_offset       :: Ogg_Int64_t
         , ovfReady_state      :: #type int
         , ovfCurrent_serialno :: #type long
         , ovfCurrent_link     :: #type long
         , ovfBittrack         :: Ogg_Int64_t
         , ovfSamptrack        :: Ogg_Int64_t
         , ovfOs               :: OggStreamState
         , ovfVd               :: VorbisDspState
         , ovfVb               :: VorbisBlock
         , ovfCallbacks        :: OvCallbacks a
         }
       deriving Show

instance Storable (OggVorbisFile a) where
  sizeOf _    = #size      OggVorbis_File
  alignment _ = #alignment OggVorbis_File

  peek ptr =
    OggVorbisFile
      <$> #{peek OggVorbis_File, datasource      } ptr
      <*> #{peek OggVorbis_File, seekable        } ptr
      <*> #{peek OggVorbis_File, offset          } ptr
      <*> #{peek OggVorbis_File, end             } ptr
      <*> #{peek OggVorbis_File, oy              } ptr
      <*> #{peek OggVorbis_File, links           } ptr
      <*> #{peek OggVorbis_File, offsets         } ptr
      <*> #{peek OggVorbis_File, dataoffsets     } ptr
      <*> #{peek OggVorbis_File, serialnos       } ptr
      <*> #{peek OggVorbis_File, pcmlengths      } ptr
      <*> #{peek OggVorbis_File, vi              } ptr
      <*> #{peek OggVorbis_File, vc              } ptr
      <*> #{peek OggVorbis_File, pcm_offset      } ptr
      <*> #{peek OggVorbis_File, ready_state     } ptr
      <*> #{peek OggVorbis_File, current_serialno} ptr
      <*> #{peek OggVorbis_File, current_link    } ptr
      <*> #{peek OggVorbis_File, bittrack        } ptr
      <*> #{peek OggVorbis_File, samptrack       } ptr
      <*> #{peek OggVorbis_File, os              } ptr
      <*> #{peek OggVorbis_File, vd              } ptr
      <*> #{peek OggVorbis_File, vb              } ptr
      <*> #{peek OggVorbis_File, callbacks       } ptr

  poke ptr val = do
    #{poke OggVorbis_File, datasource      } ptr $ val & ovfDatasource
    #{poke OggVorbis_File, seekable        } ptr $ val & ovfSeekable
    #{poke OggVorbis_File, offset          } ptr $ val & ovfOffset
    #{poke OggVorbis_File, end             } ptr $ val & ovfEnd
    #{poke OggVorbis_File, oy              } ptr $ val & ovfOy
    #{poke OggVorbis_File, links           } ptr $ val & ovfLinks
    #{poke OggVorbis_File, offsets         } ptr $ val & ovfOffsets
    #{poke OggVorbis_File, dataoffsets     } ptr $ val & ovfDataoffsets
    #{poke OggVorbis_File, serialnos       } ptr $ val & ovfSerialnos
    #{poke OggVorbis_File, pcmlengths      } ptr $ val & ovfPcmlengths
    #{poke OggVorbis_File, vi              } ptr $ val & ovfVi
    #{poke OggVorbis_File, vc              } ptr $ val & ovfVc
    #{poke OggVorbis_File, pcm_offset      } ptr $ val & ovfPcm_offset
    #{poke OggVorbis_File, ready_state     } ptr $ val & ovfReady_state
    #{poke OggVorbis_File, current_serialno} ptr $ val & ovfCurrent_serialno
    #{poke OggVorbis_File, current_link    } ptr $ val & ovfCurrent_link
    #{poke OggVorbis_File, bittrack        } ptr $ val & ovfBittrack
    #{poke OggVorbis_File, samptrack       } ptr $ val & ovfSamptrack
    #{poke OggVorbis_File, os              } ptr $ val & ovfOs
    #{poke OggVorbis_File, vd              } ptr $ val & ovfVd
    #{poke OggVorbis_File, vb              } ptr $ val & ovfVb
    #{poke OggVorbis_File, callbacks       } ptr $ val & ovfCallbacks



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

          seekFunc ptr offset origin = do
            mem@(OvMemoryPointer _ total current) <- peek ptr
            let mayNewCurrent =
                  case origin of
                    #{const SEEK_SET} -> Just $ fromIntegral offset
                    #{const SEEK_CUR} -> Just $ fromIntegral offset + fromIntegral current
                    #{const SEEK_END} -> Just $ fromIntegral offset + total
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
