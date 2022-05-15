{-# LANGUAGE DataKinds
           , FlexibleInstances
           , ForeignFunctionInterface
           , PatternSynonyms
           , TemplateHaskell
           , TypeApplications #-}

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
import           Data.Field
import           Data.Field.Storable.TH

import           Data.Coerce
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

deriveStorable1 #{offset OggVorbis_File, datasource      } "ovfDatasource"       ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, seekable        } "ovfSeekable"         ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, offset          } "ovfOffset"           ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, end             } "ovfEnd"              ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, oy              } "ovfOy"               ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, links           } "ovfLinks"            ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, offsets         } "ovfOffsets"          ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, dataoffsets     } "ovfDataoffsets"      ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, serialnos       } "ovfSerialnos"        ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, pcmlengths      } "ovfPcmlengths"       ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, vi              } "ovfVi"               ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, vc              } "ovfVc"               ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, pcm_offset      } "ovfPcm_offset"       ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, ready_state     } "ovfReady_state"      ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, current_serialno} "ovfCurrent_serialno" ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, current_link    } "ovfCurrent_link"     ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, bittrack        } "ovfBittrack"         ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, samptrack       } "ovfSamptrack"        ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, os              } "ovfOs"               ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, vd              } "ovfVd"               ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, vb              } "ovfVb"               ''OggVorbisFile
deriveStorable1 #{offset OggVorbis_File, callbacks       } "ovfCallbacks"        ''OggVorbisFile

instance Storable (OggVorbisFile a) where
  sizeOf _    = #size      OggVorbis_File
  alignment _ = #alignment OggVorbis_File

  peek ptr =
    OggVorbisFile
      <$> peekField @"ovfDatasource"       ptr
      <*> peekField @"ovfSeekable"         ptr
      <*> peekField @"ovfOffset"           ptr
      <*> peekField @"ovfEnd"              ptr
      <*> peekField @"ovfOy"               ptr
      <*> peekField @"ovfLinks"            ptr
      <*> peekField @"ovfOffsets"          ptr
      <*> peekField @"ovfDataoffsets"      ptr
      <*> peekField @"ovfSerialnos"        ptr
      <*> peekField @"ovfPcmlengths"       ptr
      <*> peekField @"ovfVi"               ptr
      <*> peekField @"ovfVc"               ptr
      <*> peekField @"ovfPcm_offset"       ptr
      <*> peekField @"ovfReady_state"      ptr
      <*> peekField @"ovfCurrent_serialno" ptr
      <*> peekField @"ovfCurrent_link"     ptr
      <*> peekField @"ovfBittrack"         ptr
      <*> peekField @"ovfSamptrack"        ptr
      <*> peekField @"ovfOs"               ptr
      <*> peekField @"ovfVd"               ptr
      <*> peekField @"ovfVb"               ptr
      <*> peekField @"ovfCallbacks"        ptr

  poke ptr val = do
    pokeRecordField @"ovfDatasource"       ptr val
    pokeRecordField @"ovfSeekable"         ptr val
    pokeRecordField @"ovfOffset"           ptr val
    pokeRecordField @"ovfEnd"              ptr val
    pokeRecordField @"ovfOy"               ptr val
    pokeRecordField @"ovfLinks"            ptr val
    pokeRecordField @"ovfOffsets"          ptr val
    pokeRecordField @"ovfDataoffsets"      ptr val
    pokeRecordField @"ovfSerialnos"        ptr val
    pokeRecordField @"ovfPcmlengths"       ptr val
    pokeRecordField @"ovfVi"               ptr val
    pokeRecordField @"ovfVc"               ptr val
    pokeRecordField @"ovfPcm_offset"       ptr val
    pokeRecordField @"ovfReady_state"      ptr val
    pokeRecordField @"ovfCurrent_serialno" ptr val
    pokeRecordField @"ovfCurrent_link"     ptr val
    pokeRecordField @"ovfBittrack"         ptr val
    pokeRecordField @"ovfSamptrack"        ptr val
    pokeRecordField @"ovfOs"               ptr val
    pokeRecordField @"ovfVd"               ptr val
    pokeRecordField @"ovfVb"               ptr val
    pokeRecordField @"ovfCallbacks"        ptr val



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
