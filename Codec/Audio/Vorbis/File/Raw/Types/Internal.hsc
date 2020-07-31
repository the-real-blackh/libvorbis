{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.File.Raw.Types.Internal where

import           Codec.Container.Ogg.Raw.Types

import           Data.Function ((&))
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

instance Storable (OvCallbacks a) where
  sizeOf _    = #size      ov_callbacks
  alignment _ = #alignment ov_callbacks

  peek ptr =
    OvCallbacks
      <$> #{peek ov_callbacks, read_func } ptr
      <*> #{peek ov_callbacks, seek_func } ptr
      <*> #{peek ov_callbacks, close_func} ptr
      <*> #{peek ov_callbacks, tell_func } ptr

  poke ptr val = do
    #{poke ov_callbacks, read_func } ptr $ val & ovRead_func
    #{poke ov_callbacks, seek_func } ptr $ val & ovSeek_func
    #{poke ov_callbacks, close_func} ptr $ val & ovClose_func
    #{poke ov_callbacks, tell_func } ptr $ val & ovTell_func



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

instance Storable (OvMemoryPointer a) where
  sizeOf _    = sizeOf (undefined :: Ptr a) + 2 * sizeOf (undefined :: #{type size_t})
  alignment _ = alignment (undefined :: Ptr a)
  
  peek ptr =
    OvMemoryPointer
      <$> peek (castPtr ptr)                               
      <*> peek (ptr `plusPtr` sizeOf (undefined :: Ptr a)) 
      <*> peek (ptr `plusPtr` (sizeOf (undefined :: Ptr a) + sizeOf (undefined :: #{type size_t})))
  
  poke ptr (OvMemoryPointer a b c) = do
    poke (castPtr ptr) a
    poke (ptr `plusPtr` sizeOf (undefined :: Ptr a)) b
    poke (ptr `plusPtr` (sizeOf (undefined :: Ptr a) + sizeOf (undefined :: #{type size_t}))) c
