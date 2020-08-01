module Codec.Audio.Vorbis.File.Raw.Seeking where

import           Codec.Audio.Vorbis.File.Raw.Seeking.Internal
import           Codec.Audio.Vorbis.File.Raw.Types
import           Codec.Audio.Vorbis.Raw.Internal.Exception
import           Codec.Container.Ogg.Raw.Types

import           Data.Int
import           Foreign.Ptr

-- | Throws 'VorbisError' on negative result.
ov_raw_seek :: Ptr (OggVorbisFile a) -> #{type long} -> IO ()
ov_raw_seek vf pos =
  ovError "ov_raw_seek" $
    ov_raw_seek' vf pos



-- | Throws 'VorbisError' on negative result.
ov_pcm_seek :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO ()
ov_pcm_seek vf pos =
  ovError "ov_pcm_seek" $
    ov_pcm_seek' vf pos



-- | Throws 'VorbisError' on negative result.
ov_time_seek :: Ptr (OggVorbisFile a) -> #{type double} -> IO ()
ov_time_seek vf s =
  ovError "ov_time_seek" $
    ov_time_seek' vf s



-- | Throws 'VorbisError' on negative result.
ov_pcm_seek_page :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO ()
ov_pcm_seek_page vf pos =
  ovError "ov_raw_seek_page" $
    ov_pcm_seek_page' vf pos



-- | Throws 'VorbisError' on negative result.
ov_time_seek_page :: Ptr (OggVorbisFile a) -> #{type double} -> IO ()
ov_time_seek_page vf s =
  ovError "ov_time_seek_page" $
    ov_time_seek_page' vf s



-- | Throws 'VorbisError' on negative result.
ov_raw_seek_lap :: Ptr (OggVorbisFile a) -> #{type long} -> IO ()
ov_raw_seek_lap vf pos =
  ovError "ov_raw_seek_lap" $
    ov_raw_seek_lap' vf pos



-- | Throws 'VorbisError' on negative result.
ov_pcm_seek_lap :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO ()
ov_pcm_seek_lap vf pos =
  ovError "ov_pcm_seek_lap" $
    ov_pcm_seek_lap' vf pos



-- | Throws 'VorbisError' on negative result.
ov_time_seek_lap :: Ptr (OggVorbisFile a) -> #{type double} -> IO ()
ov_time_seek_lap vf s =
  ovError "ov_time_seek_lap" $
    ov_time_seek_lap' vf s



-- | Throws 'VorbisError' on negative result.
ov_pcm_seek_page_lap :: Ptr (OggVorbisFile a) -> Ogg_Int64_t -> IO ()
ov_pcm_seek_page_lap vf pos =
  ovError "ov_raw_seek_page_lap" $
    ov_pcm_seek_page_lap' vf pos



-- | Throws 'VorbisError' on negative result.
ov_time_seek_page_lap :: Ptr (OggVorbisFile a) -> #{type double} -> IO ()
ov_time_seek_page_lap vf s =
  ovError "ov_time_seek_page_lap" $
    ov_time_seek_page_lap' vf s
