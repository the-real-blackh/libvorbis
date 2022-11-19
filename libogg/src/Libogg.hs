-- | The API reference can be found [here](https://xiph.org/ogg/doc/libogg/).

module Libogg
  ( -- ** Data Structures
    Ogg_Int64_t
  , OggpackBuffer (..)
  , OggPage (..)
  , OggStreamState (..)
  , OggPacket (..)
  , OggSyncState (..)
    -- ** Bitpacking
  , oggpack_writeinit
  , oggpack_writecheck
  , oggpack_reset
  , oggpack_writetrunc
  , oggpack_writealign
  , oggpack_writecopy
  , oggpack_writeclear
  , oggpack_readinit
  , oggpack_write
  , oggpack_look
  , oggpack_look1
  , oggpack_adv
  , oggpack_adv1
  , oggpack_read
  , oggpack_read1
  , oggpack_bytes
  , oggpack_bits
  , oggpack_get_buffer
    -- ** Decoding-Related
  , ogg_sync_init
  , ogg_sync_check
  , ogg_sync_clear
  , ogg_sync_destroy
  , ogg_sync_reset
  , ogg_sync_buffer
  , ogg_sync_wrote
  , ogg_sync_pageseek
  , ogg_sync_pageout
  , ogg_stream_pagein
  , ogg_stream_packetout
  , ogg_stream_packetpeek
    -- ** Encoding-Related
  , ogg_stream_packetin
  , ogg_stream_pageout
  , ogg_stream_pageout_fill
  , ogg_stream_flush
  , ogg_stream_flush_fill
    -- ** General
  , ogg_stream_init
  , ogg_stream_check
  , ogg_stream_clear
  , ogg_stream_reset
  , ogg_stream_reset_serialno
  , ogg_stream_destroy
  , ogg_page_version
  , ogg_page_continued
  , ogg_page_packets
  , ogg_page_bos
  , ogg_page_eos
  , ogg_page_granulepos
  , ogg_page_serialno
  , ogg_page_pageno
  , ogg_packet_clear
  , ogg_page_checksum_set
  ) where

import           Libogg.Bitpacking
import           Libogg.Decoding
import           Libogg.Encoding
import           Libogg.General
import           Libogg.Types
