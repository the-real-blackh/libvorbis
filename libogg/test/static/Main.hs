module Main where

import           Bind

import           Libogg



main :: IO ()
main
  | () == ()  = return ()
  | otherwise = do
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
