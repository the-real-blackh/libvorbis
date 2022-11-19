module Main where

import           Bind

import           Libvorbis
import           Vorbisenc
import           Vorbisfile



main :: IO ()
main
  | () == ()  = return ()
  | otherwise = do
      -- Codec.Audio.Vorbis.Enc.Raw
      _ <- bind vorbis_encode_ctl
      _ <- bind vorbis_encode_init
      _ <- bind vorbis_encode_init_vbr
      _ <- bind vorbis_encode_setup_init
      _ <- bind vorbis_encode_setup_managed
      _ <- bind vorbis_encode_setup_vbr

      -- Codec.Audio.Vorbis.File.Raw
      _ <- bind poke_OV_CALLBACKS_DEFAULT
      _ <- bind poke_OV_CALLBACKS_NOCLOSE
      _ <- bind poke_OV_CALLBACKS_STREAMONLY
      _ <- bind poke_OV_CALLBACKS_STREAMONLY_NOCLOSE

      -- Setup/Teardown
      _ <- bind ov_fopen
      _ <- bind ov_open
      _ <- bind ov_open_callbacks
      _ <- bind ov_clear
      _ <- bind ov_test
      _ <- bind ov_test_callbacks
      _ <- bind ov_test_open

      -- Decoding
      _ <- bind ov_read
      _ <- bind ov_read_float
      _ <- bind ov_read_filter
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
      _ <- bind vorbis_version_string

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
      _ <- bind vorbis_comment_add_tag
      _ <- bind vorbis_comment_clear
      _ <- bind vorbis_comment_init
      _ <- bind vorbis_comment_query
      _ <- bind vorbis_comment_query_count
      _ <- bind vorbis_commentheader_out

      return ()
