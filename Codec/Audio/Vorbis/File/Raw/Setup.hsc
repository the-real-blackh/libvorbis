{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Audio.Vorbis.File.Raw.Setup where

import           Codec.Audio.Vorbis.File.Raw.Setup.Internal
import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Audio.Vorbis.File.Raw.Types

import           Control.Monad (void)
import           Control.Exception
import           Data.Coerce
import           Data.Int
import           Data.Maybe (fromMaybe)
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr

#include "vorbis/vorbisfile.h"

-- | Throws 'VorbisError' on negative result.
ov_fopen :: Ptr (OggVorbisFile ()) -> FilePath -> IO ()
ov_fopen vf path =
  ovError "ov_fopen" .
    withCString path $ \pathPtr ->
      ov_fopen' (coerce pathPtr) vf



-- | Wrapper over 'ov_fopen' and 'ov_clear'. The passed in 'OggVorbisFile' pointer
--   should not be used after this function terminates.
ov_fwith :: FilePath -> (Ptr (OggVorbisFile ()) -> IO a) -> IO a
ov_fwith path action =
  alloca $ \vf ->
    bracket_ (ov_fopen vf path)
             (ov_clear vf)
             (action vf)



#ifndef windows_HOST_OS
-- | I don't know what the proper way to convert a 'Handle' to 'Ptr' is, if you truly
--   need this feature, you'll have to do it yourself.
--
--   This function is not available under Windows.
--
--   Throws 'VorbisError' on negative result.
ov_open
  :: Ptr (OggVorbisFile ())
  -> Ptr ()                   -- ^ datasource
  -> Maybe (Ptr #{type char}) -- ^ initial
  -> Maybe #{type long}       -- ^ ibytes
  -> IO ()
ov_open vf f mayInitial mayIbytes =
  ovError "ov_open" $
    ov_open' f vf (fromMaybe nullPtr mayInitial) (fromMaybe 0 mayIbytes)



-- | Wrapper over 'ov_open' and 'ov_clear'. The passed in 'OggVorbisFile' pointer
--   should not be used after this function terminates.
ov_with
  :: Ptr ()                           -- ^ datasource
  -> Maybe (Ptr #{type char})         -- ^ initial
  -> Maybe #{type long}               -- ^ ibytes
  -> IO a                             -- ^ action if 'ov_open' fails
  -> (Ptr (OggVorbisFile ()) -> IO a) -- ^ action if 'ov_open' succeeds
  -> IO a
ov_with f mayInitial mayIbytes onFail action =
  alloca $ \vf -> do
    mayFailed <- try $ ov_open vf f mayInitial mayIbytes
    case mayFailed of
      Left (_ :: VorbisError) -> onFail
      Right _                 -> finally (action vf) (ov_clear vf)
#endif



-- | Throws 'VorbisError' on negative result.
ov_open_callbacks
  :: Ptr (OggVorbisFile a)
  -> Ptr a                    -- ^ datasource
  -> Maybe (Ptr #{type char}) -- ^ initial
  -> Maybe #{type long}       -- ^ ibytes
  -> OvCallbacks a
  -> IO ()
ov_open_callbacks vf datasource mayInitial mayIbytes callbacks =
  ovError "ov_open_callbacks" .
    with callbacks $
      ov_open_callbacks' datasource vf (fromMaybe nullPtr mayInitial) (fromMaybe 0 mayIbytes)



-- | Wrapper over 'ov_open_callbacks' and 'ov_clear'. The passed in 'OggVorbisFile' pointer
--   should not be used after this function terminates.
ov_with_callbacks
  :: Ptr a                           -- ^ datasource
  -> Maybe (Ptr #{type char})        -- ^ initial
  -> Maybe #{type long}              -- ^ ibytes
  -> OvCallbacks a
  -> IO b                            -- ^ action if 'ov_open_callbacks' fails
  -> (Ptr (OggVorbisFile a) -> IO b) -- ^ action if 'ov_open_callbacks' succeeds
  -> IO b
ov_with_callbacks datasource mayInitial mayIbytes callbacks onFail action =
  alloca $ \vf -> do
    mayFailed <- try $ ov_open_callbacks vf datasource mayInitial mayIbytes callbacks
    case mayFailed of
      Left (_ :: VorbisError) -> onFail
      Right _                 -> finally (action vf) (ov_clear vf)



-- | Result is always zero and thus is dropped.
ov_clear :: Ptr (OggVorbisFile a) -> IO ()
ov_clear = void . ov_clear'



#ifndef windows_HOST_OS
-- | Same problems as with 'ov_open'.
--
--   Throws 'VorbisError' on negative result.
ov_test
  :: Ptr (OggVorbisFile ())
  -> Ptr ()                   -- ^ datasource
  -> Maybe (Ptr #{type char}) -- ^ initial
  -> Maybe #{type long}       -- ^ ibytes
  -> IO ()
ov_test vf f mayInitial mayIbytes =
  ovError "ov_test" $
    ov_test' f vf (fromMaybe nullPtr mayInitial) (fromMaybe 0 mayIbytes)



-- | Wrapper over 'ov_test' and 'ov_clear'. The passed in 'OggVorbisFile' pointer
--   should not be used after this function terminates.
ov_test_with
  :: Ptr ()                           -- ^ datasource
  -> Maybe (Ptr #{type char})         -- ^ initial
  -> Maybe #{type long}               -- ^ ibytes
  -> IO a                             -- ^ action on 'ov_test' failure
  -> (Ptr (OggVorbisFile ()) -> IO a) -- ^ action on 'ov_test' success
  -> IO a
ov_test_with f mayInitial mayIbytes onFail action =
  alloca $ \vf -> do
    mayFailed <- try $ ov_test vf f mayInitial mayIbytes
    case mayFailed of
      Left (_ :: VorbisError) -> onFail
      Right _                 -> finally (action vf) (ov_clear vf)
#endif



-- | Throws 'VorbisError' on negative result.
ov_test_callbacks
  :: Ptr (OggVorbisFile a)
  -> Ptr a                    -- ^ datasource
  -> Maybe (Ptr #{type char}) -- ^ initial
  -> Maybe #{type long}       -- ^ ibytes
  -> OvCallbacks a
  -> IO ()
ov_test_callbacks vf datasource mayInitial mayIbytes callbacks =
  ovError "ov_open_callbacks" .
    with callbacks $
      ov_test_callbacks' datasource vf (fromMaybe nullPtr mayInitial) (fromMaybe 0 mayIbytes)



-- | Wrapper over 'ov_test_callbacks' and 'ov_clear'. The passed in 'OggVorbisFile' pointer
--   should not be used after this function terminates.
ov_test_with_callbacks
  :: Ptr a                           -- ^ datasource
  -> Maybe (Ptr #{type char})        -- ^ initial
  -> Maybe #{type long}              -- ^ ibytes
  -> OvCallbacks a
  -> IO b                            -- ^ action on 'ov_test_callbacks' failure
  -> (Ptr (OggVorbisFile a) -> IO b) -- ^ action on 'ov_test_callbacks' success
  -> IO b
ov_test_with_callbacks datasource mayInitial mayIbytes callbacks onFail action =
  alloca $ \vf -> do
    mayFailed <- try $ ov_test_callbacks vf datasource mayInitial mayIbytes callbacks
    case mayFailed of
      Left (_ :: VorbisError) -> onFail
      Right _                 -> finally (action vf) (ov_clear vf)



-- | Throws 'VorbisError' on negative result.
ov_test_open :: Ptr (OggVorbisFile a) -> IO ()
ov_test_open =
  ovError "ov_test_open" .
    ov_test_open'
