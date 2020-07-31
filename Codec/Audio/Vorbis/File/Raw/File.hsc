{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.File.Raw.File where

import           Codec.Audio.Vorbis.File.Raw.File.Internal
import           Codec.Audio.Vorbis.File.Raw.Types
import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Data.Maybe (fromMaybe)
import           Foreign.Ptr



-- | Throws 'VorbisError' on negative result.
ov_bitrate :: Ptr (OggVorbisFile a) -> #{type int} -> IO #type long
ov_bitrate vf i = do
  res <- ov_bitrate' vf i
  if res >= 0
    then return res
    else ovError' "ov_bitrate" $ fromIntegral res



-- | Returns 'Nothing' instead of @0@.
--
--   Throws 'VorbisError' on negative result.
ov_bitrate_instant :: Ptr (OggVorbisFile a) -> IO (Maybe #{type long})
ov_bitrate_instant vf = do
  res <- ov_bitrate_instant' vf
  case () of
    () | res == 0  -> return Nothing
       | res > 0   -> return $ Just res
       | otherwise -> ovError' "ov_bitrate_instant" $ fromIntegral res



foreign import ccall "ov_streams"
  ov_streams :: Ptr (OggVorbisFile a) -> IO #type long




-- | Returns 'True' if the file is seekable.
ov_seekable :: Ptr (OggVorbisFile a) -> IO Bool
ov_seekable vf =
  (/= 0) <$> ov_seekable' vf



-- | Returns 'Nothing' if the specified logical bitstream doesn't exist.
ov_serialnumber :: Ptr (OggVorbisFile a) -> Maybe #{type int} -> IO (Maybe #type long)
ov_serialnumber vf mayI = do
  res <- ov_serialnumber' vf $ fromMaybe (-1) mayI
  case res of
    -1 -> return Nothing
    _  -> return $ Just res



-- | Throws 'VorbisError' on negative result.
ov_raw_total :: Ptr (OggVorbisFile a) -> Maybe #{type int} -> IO Ogg_Int64_t
ov_raw_total vf mayI = do
  res <- ov_raw_total' vf $ fromMaybe (-1) mayI
  if res >= 0
    then return res
    else ovError' "ov_raw_total" $ fromIntegral res



-- | Throws 'VorbisError' on negative result.
ov_pcm_total :: Ptr (OggVorbisFile a) -> Maybe #{type int} -> IO Ogg_Int64_t
ov_pcm_total vf mayI = do
  res <- ov_pcm_total' vf $ fromMaybe (-1) mayI
  if res >= 0
    then return res
    else ovError' "ov_raw_total" $ fromIntegral res



-- | Throws 'VorbisError' on negative result.
ov_time_total :: Ptr (OggVorbisFile a) -> Maybe #{type int} -> IO #type double
ov_time_total vf mayI = do
  res <- ov_time_total' vf $ fromMaybe (-1) mayI
  if res >= 0
    then return res
    else ovError' "ov_raw_total" $ round res



-- | Throws 'VorbisError' on negative result.
ov_raw_tell :: Ptr (OggVorbisFile a) -> IO Ogg_Int64_t
ov_raw_tell vf = do
  res <- ov_raw_tell' vf
  if res >= 0
    then return res
    else ovError' "ov_raw_tell" $ fromIntegral res



-- | Throws 'VorbisError' on negative result.
ov_pcm_tell :: Ptr (OggVorbisFile a) -> IO Ogg_Int64_t
ov_pcm_tell vf = do
  res <- ov_pcm_tell' vf
  if res >= 0
    then return res
    else ovError' "ov_pcm_tell" $ fromIntegral res



-- | Throws 'VorbisError' on negative result.
ov_time_tell :: Ptr (OggVorbisFile a) -> IO #type double
ov_time_tell vf = do
  res <- ov_time_tell' vf
  if res >= 0
    then return res
    else ovError' "ov_time_tell" $ round res



-- | Returns 'Nothing' instead of 'nullPtr'.
ov_info :: Ptr (OggVorbisFile a) -> Maybe #{type int} -> IO (Maybe (Ptr VorbisInfo))
ov_info vf mayLink = do
  res <- ov_info' vf $ fromMaybe (-1) mayLink
  return $ if res /= nullPtr
             then Just res
             else Nothing



-- | Returns 'Nothing' instead of 'nullPtr'.
ov_comment :: Ptr (OggVorbisFile a) -> Maybe #{type int} -> IO (Maybe (Ptr VorbisComment))
ov_comment vf mayLink = do
  res <- ov_comment' vf $ fromMaybe (-1) mayLink
  return $ if res /= nullPtr
             then Just res
             else Nothing
