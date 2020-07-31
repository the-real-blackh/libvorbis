module Main where

import           Dynamic.Bind

import           Codec.Audio.Vorbis.Enc.Raw
import           Codec.Audio.Vorbis.Enc.Raw.Internal
import           Codec.Audio.Vorbis.File.Raw
import           Codec.Audio.Vorbis.File.Raw.Internal
import           Codec.Audio.Vorbis.Raw
import           Codec.Audio.Vorbis.Raw.Internal
import           Codec.Container.Ogg.Raw

import           Data.List (intercalate)



main :: IO ()
main = omegabind False

-- | This checks for whether the functions are bound correctly, however we don't actually
--   need to execute that path (and if we did it'd segfault), so we only print pure ones.
omegabind :: Bool -> IO ()
omegabind False = do
  putStrLn
    $ intercalate "\n"
        [ ""
        , "OV_FALSE\                     \ = " <> show (OV_FALSE      :: Int)
        , "OV_EOF\                       \ = " <> show (OV_EOF        :: Int)
        , "OV_HOLE\                      \ = " <> show (OV_HOLE       :: Int)
        , "OV_EREAD\                     \ = " <> show (OV_EREAD      :: Int)
        , "OV_EFAULT\                    \ = " <> show (OV_EFAULT     :: Int)
        , "OV_EIMPL\                     \ = " <> show (OV_EIMPL      :: Int)
        , "OV_EINVAL\                    \ = " <> show (OV_EINVAL     :: Int)
        , "OV_ENOTVORBIS\                \ = " <> show (OV_ENOTVORBIS :: Int)
        , "OV_EBADHEADER\                \ = " <> show (OV_EBADHEADER :: Int)
        , "OV_EVERSION\                  \ = " <> show (OV_EVERSION   :: Int)
        , "OV_ENOTAUDIO\                 \ = " <> show (OV_ENOTAUDIO  :: Int)
        , "OV_EBADPACKET\                \ = " <> show (OV_EBADPACKET :: Int)
        , "OV_EBADLINK\                  \ = " <> show (OV_EBADLINK   :: Int)
        , "OV_ENOSEEK\                   \ = " <> show (OV_ENOSEEK    :: Int)
        , ""
        , "OV_CALLBACKS_DEFAULT\         \ = " <> show  OV_CALLBACKS_DEFAULT
        , "OV_CALLBACKS_NOCLOSE\         \ = " <> show  OV_CALLBACKS_NOCLOSE
        , "OV_CALLBACKS_STREAMONLY\      \ = " <> show  OV_CALLBACKS_STREAMONLY
        , "OV_CALLBACKS_STREAMONLY_NOCLOSE = " <> show  OV_CALLBACKS_STREAMONLY_NOCLOSE
        , ""
        , "vorbis_version_string\        \ = " <> show  vorbis_version_string
        ]

omegabind True  = do
  -- Codec.Audio.Vorbis.Enc.Raw
  _ <- bind vorbis_encode_ctl'
  _ <- bind vorbis_encode_init
  _ <- bind vorbis_encode_init_vbr
  _ <- bind vorbis_encode_setup_init
  _ <- bind vorbis_encode_setup_managed
  _ <- bind vorbis_encode_setup_vbr

  -- Codec.Audio.Vorbis.File.Raw
  -- Setup/Teardown
  _ <- bind ov_fopen
  #ifndef windows_HOST_OS
  _ <- bind ov_open
  #endif
  _ <- bind ov_open_callbacks'
  print $ OV_CALLBACKS_DEFAULT
  _ <- bind ov_clear
  #ifndef windows_HOST_OS
  _ <- bind ov_test
  #endif
  _ <- bind ov_test_callbacks'
  _ <- bind ov_test_open

  -- Decoding
  _ <- bind ov_read'
  _ <- bind ov_read_float'
  _ <- bind ov_read_filter'
  _ <- bind ov_crosslap

  -- Seeking
  _ <- bind ov_raw_seek
  _ <- bind ov_pcm_seek
  _ <- bind ov_time_seek
  _ <- bind ov_pcm_seek_page
  _ <- bind ov_time_seek_page
  _ <- bind ov_raw_seek_lap
  _ <- bind ov_pcm_seek_lap
  _ <- bind ov_time_seek_lap
  _ <- bind ov_pcm_seek_page_lap
  _ <- bind ov_time_seek_page_lap

  -- File Information
  _ <- bind ov_bitrate
  _ <- bind ov_bitrate_instant
  _ <- bind ov_streams
  _ <- bind ov_seekable
  _ <- bind ov_serialnumber
  _ <- bind ov_raw_total
  _ <- bind ov_pcm_total
  _ <- bind ov_time_total
  _ <- bind ov_raw_tell
  _ <- bind ov_pcm_tell
  _ <- bind ov_time_tell
  _ <- bind ov_info
  _ <- bind ov_comment

  -- Codec.Audio.Vorbis.Raw
  -- Functions used by both decode and encode
  _ <- bind vorbis_block_clear
  _ <- bind vorbis_block_init
  _ <- bind vorbis_dsp_clear
  _ <- bind vorbis_granule_time
  _ <- bind vorbis_info_blocksize
  _ <- bind vorbis_info_clear
  _ <- bind vorbis_info_init

  -- Decoding
  _ <- bind vorbis_packet_blocksize
  _ <- bind vorbis_synthesis
  _ <- bind vorbis_synthesis_blockin
  _ <- bind vorbis_synthesis_halfrate
  _ <- bind vorbis_synthesis_halfrate_p
  _ <- bind vorbis_synthesis_headerin
  _ <- bind vorbis_synthesis_idheader
  _ <- bind vorbis_synthesis_init
  _ <- bind vorbis_synthesis_lapout
  _ <- bind vorbis_synthesis_pcmout
  _ <- bind vorbis_synthesis_read
  _ <- bind vorbis_synthesis_restart
  _ <- bind vorbis_synthesis_trackonly

  -- Encoding
  _ <- bind vorbis_analysis
  _ <- bind vorbis_analysis_blockout
  _ <- bind vorbis_analysis_buffer
  _ <- bind vorbis_analysis_headerout
  _ <- bind vorbis_analysis_init
  _ <- bind vorbis_analysis_wrote
  _ <- bind vorbis_bitrate_addblock
  _ <- bind vorbis_bitrate_flushpacket

  -- Metadata
  _ <- bind vorbis_comment_add
  _ <- bind vorbis_comment_add_tag'
  _ <- bind vorbis_comment_clear
  _ <- bind vorbis_comment_init
  _ <- bind vorbis_comment_query
  _ <- bind vorbis_comment_query_count
  _ <- bind vorbis_commentheader_out

  -- Codec.Container.Ogg.Raw
  -- Bitpacking
  _ <- bind oggpack_writeinit
  _ <- bind oggpack_writecheck
  _ <- bind oggpack_reset
  _ <- bind oggpack_writetrunc
  _ <- bind oggpack_writealign
  _ <- bind oggpack_writecopy
  _ <- bind oggpack_writeclear
  _ <- bind oggpack_readinit
  _ <- bind oggpack_write
  _ <- bind oggpack_look
  _ <- bind oggpack_look1
  _ <- bind oggpack_adv
  _ <- bind oggpack_adv1
  _ <- bind oggpack_read
  _ <- bind oggpack_read1
  _ <- bind oggpack_bytes
  _ <- bind oggpack_bits
  _ <- bind oggpack_get_buffer

  -- Decoding-Related
  _ <- bind ogg_sync_init
  _ <- bind ogg_sync_check
  _ <- bind ogg_sync_clear
  _ <- bind ogg_sync_destroy
  _ <- bind ogg_sync_reset
  _ <- bind ogg_sync_buffer
  _ <- bind ogg_sync_wrote
  _ <- bind ogg_sync_pageseek
  _ <- bind ogg_sync_pageout
  _ <- bind ogg_stream_pagein
  _ <- bind ogg_stream_packetout
  _ <- bind ogg_stream_packetpeek

  -- Encoding-Related
  _ <- bind ogg_stream_packetin
  _ <- bind ogg_stream_pageout
  _ <- bind ogg_stream_pageout_fill
  _ <- bind ogg_stream_flush
  _ <- bind ogg_stream_flush_fill

  -- General
  _ <- bind ogg_stream_init
  _ <- bind ogg_stream_check
  _ <- bind ogg_stream_clear
  _ <- bind ogg_stream_reset
  _ <- bind ogg_stream_reset_serialno
  _ <- bind ogg_stream_destroy
  _ <- bind ogg_page_version
  _ <- bind ogg_page_continued
  _ <- bind ogg_page_packets
  _ <- bind ogg_page_bos
  _ <- bind ogg_page_eos
  _ <- bind ogg_page_granulepos
  _ <- bind ogg_page_serialno
  _ <- bind ogg_page_pageno
  _ <- bind ogg_packet_clear
  _ <- bind ogg_page_checksum_set

  return ()
