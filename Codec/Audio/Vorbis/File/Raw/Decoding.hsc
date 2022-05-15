{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Audio.Vorbis.File.Raw.Decoding
  ( Endianness (..)
  , systemEndianness
  , ov_read
  , ov_read_float
  , FilterFunc
  , ov_read_filter
  , ov_crosslap
  , VorbisReadFormat
  ) where

import           Codec.Audio.Vorbis.File.Raw.Decoding.Internal
import           Codec.Audio.Vorbis.File.Raw.Types
import           Codec.Audio.Vorbis.Raw.Internal.Exception

import           Data.Coerce
import           Data.Int
import           Data.Proxy
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Foreign.Ptr



-- | Most processor architectures use 'LittleEndian' encoding, however you might need
--   'BigEndian' for network data transfer or file reading.
--
--   This could be sourced from @cpu@ or @memory@ packages instead, but both of those
--   aren't that widely used.
data Endianness = LittleEndian
                | BigEndian

-- | Your system 'Endianness'.
--
--   Identical to @getSystemEndianness@ from the @cpu@ package.
systemEndianness :: Endianness
#ifdef WORDS_BIGENDIAN
systemEndianness = BigEndian
#else
systemEndianness = LittleEndian
#endif



-- | Infers word size and sign from the buffer type. Keep in mind that in this case the
--   type of @v@ actually matters and data written will differ for each supported type.
--
--   Since this is supposed to be run within a loop, @bitstream@ stays a pointer in this
--   one instead of being a return value, 'alloca' it outside of the loop.
--
--   Returns 'Nothing' in case of EOF.
--
--   Throws 'VorbisError' on negative result.
ov_read
  :: VorbisReadFormat v
  => Ptr (OggVorbisFile a)
  -> Ptr v                   -- ^ buffer. Can be of types Word8\/Word16\/Int8\/Int16
  -> Endianness
  -> #{type int}             -- ^ length
  -> Ptr #{type int}         -- ^ bitstream
  -> IO (Maybe #{type long})
ov_read vf (buffer :: Ptr v) endian lengt bitstream =
  let (sign, size) = vorbisReadFormat (Proxy :: Proxy v)

      bigendianp = case endian of
                     LittleEndian -> 0
                     BigEndian    -> 1

      word = case size of
               Eight -> 1
               Sixteen -> 2

      sgned = case sign of
                Word -> 0
                Int  -> 1

  in do
       res <- ov_read' vf (castPtr buffer) lengt bigendianp word sgned bitstream
       case () of
         () | res == 0  -> return Nothing
            | res > 0   -> return $ Just res
            | otherwise -> ovError' "ov_read" $ fromIntegral res



-- | Returns 'Nothing' in case of EOF.
--
--   Throws 'VorbisError' on negative result.
ov_read_float
  :: Ptr (OggVorbisFile a)
  -> #{type int}                                        -- ^ samples
  -> Ptr #{type int}                                    -- ^ bitstream
  -> IO (Maybe (Ptr (Ptr #{type float}), #{type long}))
ov_read_float vf samples bitstream =
  alloca $ \pcm_channels -> do
    res <- ov_read_float' vf pcm_channels samples bitstream
    case () of
      () | res == 0  -> return Nothing
         | res > 0   -> do
             pcm <- peek pcm_channels
             return $ Just (pcm, res)
         | otherwise -> ovError' "ov_read_float" $ fromIntegral res



-- | Same reasoning as 'ov_read'.
--
--   Returns 'Nothing' in case of EOF.
--
--   Throws 'VorbisError' on negative result.
ov_read_filter
  :: VorbisReadFormat v
  => Ptr (OggVorbisFile a)
  -> Ptr v                   -- ^ buffer. Can be of types Word8\/Word16\/Int8\/Int16
  -> Endianness
  -> #{type int}             -- ^ length
  -> Ptr #{type int}         -- ^ bitstream
  -> FilterFunc b            -- ^ filter
  -> Ptr b                   -- ^ filter_param
  -> IO (Maybe #{type long})
ov_read_filter vf (buffer :: Ptr v) endian lengt bitstream filterFunc filter_param =
  let (sign, size) = vorbisReadFormat (Proxy :: Proxy v)

      bigendianp = case endian of
                     LittleEndian -> 0
                     BigEndian    -> 1

      word = case size of
               Eight -> 1
               Sixteen -> 2

      sgned = case sign of
                Word -> 0
                Int  -> 1

  in do
       filtr <- mkFilterFunc $ coerce filterFunc
       res <- ov_read_filter' vf (castPtr buffer) lengt bigendianp word sgned bitstream filtr (castPtr filter_param)
       case () of
         () | res == 0  -> return Nothing
            | res > 0   -> return $ Just res
            | otherwise -> ovError' "ov_read" $ fromIntegral res



-- | Throws 'VorbisError' on negative result.
ov_crosslap :: Ptr (OggVorbisFile a) -> Ptr (OggVorbisFile b) -> IO ()
ov_crosslap old new =
  ovError "ov_crosslap" $
    fromIntegral <$> ov_crosslap' old new
