{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Vorbis.Raw.Decoding where

import           Codec.Audio.Vorbis.Raw.Decoding.Internal
import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types

import           Data.Bool (bool)
import           Data.Int
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

#include "vorbis/codec.h"

-- | Throws 'VorbisError' on negative result.
vorbis_packet_blocksize :: Ptr VorbisInfo -> Ptr OggPacket -> IO #type long
vorbis_packet_blocksize vi op = do
  res <- vorbis_packet_blocksize' vi op
  if res >= 0
    then return res
    else ovError' "vorbis_packet_blocksize" $ fromIntegral res



-- | Throws 'VorbisError' on negative result.
vorbis_synthesis :: Ptr VorbisInfo -> Ptr OggPacket -> IO ()
vorbis_synthesis vb op =
  ovError "vorbis_synthesis" $ vorbis_synthesis' vb op



-- | Throws 'VorbisError' on negative result.
vorbis_synthesis_blockin :: Ptr VorbisDspState -> Ptr VorbisBlock -> IO ()
vorbis_synthesis_blockin v vb =
  ovError "vorbis_synthesis_blockin" $ vorbis_synthesis_blockin' v vb



-- | 'True' turns half-rate mode on, 'False' turns it off.
--
--   Throws 'VorbisError' on negative result.
vorbis_synthesis_halfrate :: Ptr VorbisInfo -> Bool -> IO ()
vorbis_synthesis_halfrate ptr =
  ovError "vorbis_synthesis_halfrate" .
    vorbis_synthesis_halfrate' ptr . bool 0 1



-- | Returns 'True' if half-rate mode is on.
--   
--   Throws 'VorbisError' on negative result.
vorbis_synthesis_halfrate_p :: Ptr VorbisInfo -> IO Bool
vorbis_synthesis_halfrate_p ptr = do
  res <- vorbis_synthesis_halfrate_p' ptr
  case res of
    1 -> return True
    0 -> return False
    _ -> ovError' "vorbis_synthesis_halfrate_p" res



-- | Throws 'VorbisError' on negative result.
vorbis_synthesis_headerin :: Ptr VorbisInfo -> Ptr VorbisComment -> Ptr OggPacket -> IO ()
vorbis_synthesis_headerin vi vc op =
  ovError "vorbis_synthesis_headerin" $ vorbis_synthesis_headerin' vi vc op


-- | Returns 'True' if the packet is a valid first packet.
--
--   Throws 'VorbisError' on negative result.
vorbis_synthesis_idheader :: Ptr OggPacket -> IO Bool
vorbis_synthesis_idheader op = do
  res <- vorbis_synthesis_idheader' op
  case res of
    1 -> return True
    0 -> return False
    _ -> ovError' "vorbis_synthesis_idheader" res



-- | Throws 'VorbisError' on negative result.
vorbis_synthesis_init :: Ptr VorbisDspState -> Ptr VorbisInfo -> IO ()
vorbis_synthesis_init v vi =
  ovError "vorbis_synthesis_init" $ vorbis_synthesis_init' v vi



-- | 'vorbis_synthesis_lapout'' with @pcm@ argument passed as NULL.
--   
--   Returns 'Nothing' instead of @0@.
vorbis_synthesis_lapout_samples :: Ptr VorbisDspState -> IO (Maybe #{type int})
vorbis_synthesis_lapout_samples v = do
  res <- vorbis_synthesis_lapout' v nullPtr
  case () of
    () | res == 0  -> return Nothing
       | res >  0  -> return $ Just res
       | otherwise -> ovError' "vorbis_synthesis_lapout_samples" res



-- | Returns 'Nothing' instead of @0@.
vorbis_synthesis_lapout :: Ptr VorbisDspState -> IO (Ptr (Ptr #{type float}), Maybe #{type int})
vorbis_synthesis_lapout v =
  alloca $ \pcmPtr -> do
    res <- vorbis_synthesis_lapout' v pcmPtr
    pcm <- peek pcmPtr
    case () of
      () | res == 0  -> return (pcm, Nothing)
         | res >  0  -> return (pcm, Just res)
         | otherwise -> ovError' "vorbis_synthesis_lapout" res



-- | 'vorbis_synthesis_pcmout'' with @pcm@ argument passed as NULL.
--   
--   Returns 'Nothing' instead of @0@.
vorbis_synthesis_pcmout_samples :: Ptr VorbisDspState -> IO (Maybe #{type int})
vorbis_synthesis_pcmout_samples v = do
  res <- vorbis_synthesis_pcmout' v nullPtr
  case () of
    () | res == 0  -> return Nothing
       | res >  0  -> return $ Just res
       | otherwise -> ovError' "vorbis_synthesis_pcmout_samples" res



-- | Returns 'Nothing' instead of @0@.
vorbis_synthesis_pcmout :: Ptr VorbisDspState -> IO (Ptr (Ptr #{type float}), Maybe #{type int})
vorbis_synthesis_pcmout v =
  alloca $ \pcmPtr -> do
    res <- vorbis_synthesis_pcmout' v pcmPtr
    pcm <- peek pcmPtr
    case () of
      () | res == 0  -> return (pcm, Nothing)
         | res >  0  -> return (pcm, Just res)
         | otherwise -> ovError' "vorbis_synthesis_pcmout" res



-- | Throws 'VorbisError' on negative result.
vorbis_synthesis_read :: Ptr VorbisDspState -> #{type int} -> IO ()
vorbis_synthesis_read v samples =
  ovError "vorbis_synthesis_read" $ vorbis_synthesis_read' v samples




-- | Throws 'VorbisError' on negative result.
vorbis_synthesis_restart :: Ptr VorbisDspState -> IO ()
vorbis_synthesis_restart v =
  ovError "vorbis_synthesis_restart" $ vorbis_synthesis_restart' v



-- | Throws 'VorbisError' on negative result.
vorbis_synthesis_trackonly :: Ptr VorbisBlock -> Ptr OggPacket -> IO ()
vorbis_synthesis_trackonly vb op =
  ovError "vorbis_synthesis_trackonly" $ vorbis_synthesis_trackonly' vb op
