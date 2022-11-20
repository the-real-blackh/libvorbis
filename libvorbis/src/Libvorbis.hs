{-# LANGUAGE PatternSynonyms #-}

module Libvorbis
  ( -- ** Data Structures
    Ogg_Int64_t
  , VorbisBlock (..)
  , VorbisComment (..)
  , VorbisDspState (..)
  , VorbisInfo (..)
    -- ** Functions used by both decode and encode
  , vorbis_block_clear
  , vorbis_block_init
  , vorbis_dsp_clear
  , vorbis_granule_time
  , vorbis_info_blocksize
  , vorbis_info_clear
  , vorbis_info_init
  , vorbis_version_string
    -- ** Decoding
  , vorbis_packet_blocksize
  , vorbis_synthesis
  , vorbis_synthesis_blockin
  , vorbis_synthesis_halfrate
  , vorbis_synthesis_halfrate_p
  , vorbis_synthesis_headerin
  , vorbis_synthesis_idheader
  , vorbis_synthesis_init
  , vorbis_synthesis_lapout
  , vorbis_synthesis_pcmout
  , vorbis_synthesis_read
  , vorbis_synthesis_restart
  , vorbis_synthesis_trackonly
    -- * Encoding
  , vorbis_analysis
  , vorbis_analysis_blockout
  , vorbis_analysis_buffer
  , vorbis_analysis_headerout
  , vorbis_analysis_init
  , vorbis_analysis_wrote
  , vorbis_bitrate_addblock
  , vorbis_bitrate_flushpacket
    -- ** Metadata
  , vorbis_comment_add
  , vorbis_comment_add_tag
  , vorbis_comment_clear
  , vorbis_comment_init
  , vorbis_comment_query
  , vorbis_comment_query_count
  , vorbis_commentheader_out
    -- ** Return Codes
  , pattern OV_FALSE
  , pattern OV_EOF
  , pattern OV_HOLE
  , pattern OV_EREAD
  , pattern OV_EFAULT
  , pattern OV_EIMPL
  , pattern OV_EINVAL
  , pattern OV_ENOTVORBIS
  , pattern OV_EBADHEADER
  , pattern OV_EVERSION
  , pattern OV_ENOTAUDIO
  , pattern OV_EBADPACKET
  , pattern OV_EBADLINK
  , pattern OV_ENOSEEK
  ) where

import           Libogg
import           Libvorbis.Both
import           Libvorbis.Decoding
import           Libvorbis.Encoding
import           Libvorbis.Metadata
import           Libvorbis.Return
import           Libvorbis.Types
