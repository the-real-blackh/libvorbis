{-# LANGUAGE DataKinds
           , FlexibleInstances
           , ForeignFunctionInterface
           , TemplateHaskell
           , TypeApplications #-}

module Codec.Audio.Vorbis.File.Raw.Types.Internal where

import           Codec.Container.Ogg.Raw.Types
import           Data.Field
import           Data.Field.Storable.TH

import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable

#include "vorbis/vorbisfile.h"
#include "vorbis/vorbisfile-plus.h"

-- | Reads a new chunk of the stream
type ReadFunc a = Ptr ()            -- ^ @ptr@, buffer to write to, is of size @size * nmemb@
               -> #{type size_t}    -- ^ @size@, element size in bytes
               -> #{type size_t}    -- ^ @count@, number of elements
               -> Ptr a             -- ^ @stream@, input stream of elements
               -> IO #{type size_t} -- ^ amount of bytes read. Amount not equal to @size * nmemb@ ends reading.

-- | Sets the position in the stream
type SeekFunc  a = Ptr a          -- ^ @datasource@, input stream
                -> Ogg_Int64_t    -- ^ @offset@, offset from @origin@ of @datasource@
                -> #{type int}    -- ^ @origin@, start of @datasource@
                -> IO #{type int} -- ^ 0 on success

-- | Closes the stream
type CloseFunc a = Ptr a
                -> IO #{type int} -- ^ 0 on success

-- | Reports the current position in the stream
type TellFunc a = Ptr a
               -> IO #{type long} -- ^ (-1) on failure



data OvCallbacks a =
       OvCallbacks
         { ovRead_func  :: FunPtr (ReadFunc a)
         , ovSeek_func  :: FunPtr (SeekFunc a)
         , ovClose_func :: FunPtr (CloseFunc a)
         , ovTell_func  :: FunPtr (TellFunc a)
         }
       deriving Show

deriveStorable1 #{offset ov_callbacks, read_func  } "ovRead_func"  ''OvCallbacks
deriveStorable1 #{offset ov_callbacks, seek_func  } "ovSeek_func"  ''OvCallbacks
deriveStorable1 #{offset ov_callbacks, close_func } "ovClose_func" ''OvCallbacks
deriveStorable1 #{offset ov_callbacks, tell_func  } "ovTell_func"  ''OvCallbacks

instance Storable (OvCallbacks a) where
  sizeOf _    = #size      ov_callbacks
  alignment _ = #alignment ov_callbacks

  peek ptr =
    OvCallbacks
      <$> peekField @"ovRead_func"  ptr
      <*> peekField @"ovSeek_func"  ptr
      <*> peekField @"ovClose_func" ptr
      <*> peekField @"ovTell_func"  ptr

  poke ptr val = do
    pokeRecordField @"ovRead_func"  ptr val
    pokeRecordField @"ovSeek_func"  ptr val
    pokeRecordField @"ovClose_func" ptr val
    pokeRecordField @"ovTell_func"  ptr val



foreign import ccall unsafe "ov_callbacks_default_ptr"
  ov_callbacks_default_ptr :: Ptr (OvCallbacks ()) -> IO ()

foreign import ccall unsafe "ov_callbacks_noclose_ptr"
  ov_callbacks_noclose_ptr :: Ptr (OvCallbacks ()) -> IO ()
  
foreign import ccall unsafe "ov_callbacks_streamonly_ptr"
  ov_callbacks_streamonly_ptr :: Ptr (OvCallbacks ()) -> IO ()
  
foreign import ccall unsafe "ov_callbacks_streamonly_noclose_ptr"
  ov_callbacks_streamonly_noclose_ptr :: Ptr (OvCallbacks ()) -> IO ()



foreign import ccall unsafe "wrapper"
  mkReadFunc :: ReadFunc () -> IO (FunPtr (ReadFunc ()))

foreign import ccall unsafe "wrapper"
  mkSeekFunc :: SeekFunc () -> IO (FunPtr (SeekFunc ()))

foreign import ccall unsafe "wrapper"
  mkCloseFunc :: CloseFunc () -> IO (FunPtr (CloseFunc ()))

foreign import ccall unsafe "wrapper"
  mkTellFunc :: TellFunc () -> IO (FunPtr (TellFunc ()))



data OvMemoryPointer a =
       OvMemoryPointer
         { ompDatasource :: Ptr a          -- ^ Pointer to the start of the datasource.
         , ompTotal      :: #{type size_t} -- ^ Size of the datasource
         , ompCurrent    :: #{type size_t} -- ^ Current position in the datasource.
         }

instance Storable (Field "ompDatasource" (OvMemoryPointer a)) where
  sizeOf    = foldField sizeOf
  alignment = foldField alignment

  peek ptr = mkField <$> peek (castPtr ptr)
  poke ptr = foldField $ poke (castPtr ptr)

instance Storable (Field "ompTotal" (OvMemoryPointer a)) where
  sizeOf    = foldField sizeOf
  alignment = foldField alignment

  peek ptr = mkField <$> peek (ptr `plusPtr` sizeOf (undefined :: Ptr a))
  poke ptr = foldField $ poke (ptr `plusPtr` sizeOf (undefined :: Ptr a))

instance Storable (Field "ompCurrent" (OvMemoryPointer a)) where
  sizeOf    = foldField sizeOf
  alignment = foldField alignment

  peek ptr = mkField <$> peek (plusPtr ptr $ sizeOf (undefined :: Ptr a) + sizeOf (undefined :: #{type size_t}))
  poke ptr = foldField $ poke (plusPtr ptr $ sizeOf (undefined :: Ptr a) + sizeOf (undefined :: #{type size_t}))

instance Storable (OvMemoryPointer a) where
  sizeOf _    = sizeOf (undefined :: Ptr a) + 2 * sizeOf (undefined :: #{type size_t})
  alignment _ = alignment (undefined :: Ptr a)
  
  peek ptr =
    OvMemoryPointer
      <$> peekField @"ompDatasource" ptr
      <*> peekField @"ompTotal"      ptr
      <*> peekField @"ompCurrent"    ptr
  
  poke ptr val = do
    pokeRecordField @"ompDatasource" ptr val
    pokeRecordField @"ompTotal"      ptr val
    pokeRecordField @"ompCurrent"    ptr val
