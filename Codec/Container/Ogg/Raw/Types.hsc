{-# LANGUAGE DataKinds
           , FlexibleInstances
           , ForeignFunctionInterface
           , TemplateHaskell
           , TypeApplications #-}

module Codec.Container.Ogg.Raw.Types where

import           Data.Field
import           Data.Field.Storable.TH

import           Foreign

#include "ogg/ogg.h"

type Ogg_Int64_t = #type ogg_int64_t



data OggpackBuffer =
       OggpackBuffer
         { obEndbyte :: #type long
         , obEndbit  :: #type int
         , obBuffer  :: Ptr #type unsigned char
         , obPtr     :: Ptr #type unsigned char
         , obStorage :: #type long
         }
       deriving Show

deriveStorable #{offset oggpack_buffer, endbyte} "obEndbyte" ''OggpackBuffer
deriveStorable #{offset oggpack_buffer, endbit } "obEndbit"  ''OggpackBuffer
deriveStorable #{offset oggpack_buffer, buffer } "obBuffer"  ''OggpackBuffer
deriveStorable #{offset oggpack_buffer, ptr    } "obPtr"     ''OggpackBuffer
deriveStorable #{offset oggpack_buffer, storage} "obStorage" ''OggpackBuffer

instance Storable OggpackBuffer where
  sizeOf _    = #size      oggpack_buffer
  alignment _ = #alignment oggpack_buffer

  peek ptr =
    OggpackBuffer
      <$> peekField @"obEndbyte" ptr
      <*> peekField @"obEndbit"  ptr
      <*> peekField @"obBuffer"  ptr
      <*> peekField @"obPtr"     ptr
      <*> peekField @"obStorage" ptr

  poke ptr val = do
    pokeRecordField @"obEndbyte" ptr val
    pokeRecordField @"obEndbit"  ptr val
    pokeRecordField @"obBuffer"  ptr val
    pokeRecordField @"obPtr"     ptr val
    pokeRecordField @"obStorage" ptr val



data OggPage =
       OggPage
         { opHeader     :: Ptr #type unsigned char
         , opHeader_len :: #type long
         , opBody       :: Ptr #type unsigned char
         , opBody_len   :: #type long
         }
       deriving Show

deriveStorable #{offset ogg_page, header    } "opHeader"     ''OggPage
deriveStorable #{offset ogg_page, header_len} "opHeader_len" ''OggPage
deriveStorable #{offset ogg_page, body      } "opBody"       ''OggPage
deriveStorable #{offset ogg_page, body_len  } "opBody_len"   ''OggPage

instance Storable OggPage where
  sizeOf _    = #size      ogg_page
  alignment _ = #alignment ogg_page

  peek ptr =
    OggPage
      <$> peekField @"opHeader"     ptr
      <*> peekField @"opHeader_len" ptr
      <*> peekField @"opBody"       ptr
      <*> peekField @"opBody_len"   ptr

  poke ptr val = do
    pokeRecordField @"opHeader"     ptr val
    pokeRecordField @"opHeader_len" ptr val
    pokeRecordField @"opBody"       ptr val
    pokeRecordField @"opBody_len"   ptr val



data OggStreamState =
       OggStreamState
         { ossBody_data       :: Ptr #type unsigned char
         , ossBody_storage    :: #type long
         , ossBody_fill       :: #type long
         , ossBody_returned   :: #type long
         , ossLacing_vals     :: Ptr #type int
         , ossGranule_vals    :: Ptr Ogg_Int64_t
         , ossLacing_storage  :: #type long
         , ossLacing_fill     :: #type long
         , ossLacing_packet   :: #type long
         , ossLacing_returned :: #type long
         , ossHeader          :: Ptr #type unsigned char
         , ossHeader_fill     :: #type int
         , ossE_o_s           :: #type int
         , ossB_o_s           :: #type int
         , ossSerialno        :: #type long
         , ossPageno          :: #type long
         , ossPacketno        :: Ogg_Int64_t
         , ossGranulepos      :: Ogg_Int64_t
         }
       deriving Show

deriveStorable #{offset ogg_stream_state, body_data      } "ossBody_data"       ''OggStreamState
deriveStorable #{offset ogg_stream_state, body_storage   } "ossBody_storage"    ''OggStreamState
deriveStorable #{offset ogg_stream_state, body_fill      } "ossBody_fill"       ''OggStreamState
deriveStorable #{offset ogg_stream_state, body_returned  } "ossBody_returned"   ''OggStreamState
deriveStorable #{offset ogg_stream_state, lacing_vals    } "ossLacing_vals"     ''OggStreamState
deriveStorable #{offset ogg_stream_state, granule_vals   } "ossGranule_vals"    ''OggStreamState
deriveStorable #{offset ogg_stream_state, lacing_storage } "ossLacing_storage"  ''OggStreamState
deriveStorable #{offset ogg_stream_state, lacing_fill    } "ossLacing_fill"     ''OggStreamState
deriveStorable #{offset ogg_stream_state, lacing_packet  } "ossLacing_packet"   ''OggStreamState
deriveStorable #{offset ogg_stream_state, lacing_returned} "ossLacing_returned" ''OggStreamState
deriveStorable #{offset ogg_stream_state, header         } "ossHeader"          ''OggStreamState
deriveStorable #{offset ogg_stream_state, header_fill    } "ossHeader_fill"     ''OggStreamState
deriveStorable #{offset ogg_stream_state, e_o_s          } "ossE_o_s"           ''OggStreamState
deriveStorable #{offset ogg_stream_state, b_o_s          } "ossB_o_s"           ''OggStreamState
deriveStorable #{offset ogg_stream_state, serialno       } "ossSerialno"        ''OggStreamState
deriveStorable #{offset ogg_stream_state, pageno         } "ossPageno"          ''OggStreamState
deriveStorable #{offset ogg_stream_state, packetno       } "ossPacketno"        ''OggStreamState
deriveStorable #{offset ogg_stream_state, granulepos     } "ossGranulepos"      ''OggStreamState

instance Storable OggStreamState where
  sizeOf _    = #size      ogg_stream_state
  alignment _ = #alignment ogg_stream_state

  peek ptr =
    OggStreamState
      <$> peekField @"ossBody_data"       ptr
      <*> peekField @"ossBody_storage"    ptr
      <*> peekField @"ossBody_fill"       ptr
      <*> peekField @"ossBody_returned"   ptr
      <*> peekField @"ossLacing_vals"     ptr
      <*> peekField @"ossGranule_vals"    ptr
      <*> peekField @"ossLacing_storage"  ptr
      <*> peekField @"ossLacing_fill"     ptr
      <*> peekField @"ossLacing_packet"   ptr
      <*> peekField @"ossLacing_returned" ptr
      <*> peekField @"ossHeader"          ptr
      <*> peekField @"ossHeader_fill"     ptr
      <*> peekField @"ossE_o_s"           ptr
      <*> peekField @"ossB_o_s"           ptr
      <*> peekField @"ossSerialno"        ptr
      <*> peekField @"ossPageno"          ptr
      <*> peekField @"ossPacketno"        ptr
      <*> peekField @"ossGranulepos"      ptr

  poke ptr val = do
    pokeRecordField @"ossBody_data"       ptr val
    pokeRecordField @"ossBody_storage"    ptr val
    pokeRecordField @"ossBody_fill"       ptr val
    pokeRecordField @"ossBody_returned"   ptr val
    pokeRecordField @"ossLacing_vals"     ptr val
    pokeRecordField @"ossGranule_vals"    ptr val
    pokeRecordField @"ossLacing_storage"  ptr val
    pokeRecordField @"ossLacing_fill"     ptr val
    pokeRecordField @"ossLacing_packet"   ptr val
    pokeRecordField @"ossLacing_returned" ptr val
    pokeRecordField @"ossHeader"          ptr val
    pokeRecordField @"ossHeader_fill"     ptr val
    pokeRecordField @"ossE_o_s"           ptr val
    pokeRecordField @"ossB_o_s"           ptr val
    pokeRecordField @"ossSerialno"        ptr val
    pokeRecordField @"ossPageno"          ptr val
    pokeRecordField @"ossPacketno"        ptr val
    pokeRecordField @"ossGranulepos"      ptr val



data OggPacket =
       OggPacket
         { opPacket     :: Ptr #type unsigned char
         , opBytes      :: #type long
         , opB_o_s      :: #type long
         , opE_o_s      :: #type long
         , opGranulepos :: Ogg_Int64_t
         , opPacketno   :: Ogg_Int64_t
         }
       deriving Show

deriveStorable #{offset ogg_packet, packet    } "opPacket"     ''OggPacket
deriveStorable #{offset ogg_packet, bytes     } "opBytes"      ''OggPacket
deriveStorable #{offset ogg_packet, b_o_s     } "opB_o_s"      ''OggPacket
deriveStorable #{offset ogg_packet, e_o_s     } "opE_o_s"      ''OggPacket
deriveStorable #{offset ogg_packet, granulepos} "opGranulepos" ''OggPacket
deriveStorable #{offset ogg_packet, packetno  } "opPacketno"   ''OggPacket



instance Storable OggPacket where
  sizeOf _    = #size      ogg_packet
  alignment _ = #alignment ogg_packet

  peek ptr =
    OggPacket
      <$> peekField @"opPacket"     ptr
      <*> peekField @"opBytes"      ptr
      <*> peekField @"opB_o_s"      ptr
      <*> peekField @"opE_o_s"      ptr
      <*> peekField @"opGranulepos" ptr
      <*> peekField @"opPacketno"   ptr

  poke ptr val = do
    pokeRecordField @"opPacket"     ptr val
    pokeRecordField @"opBytes"      ptr val
    pokeRecordField @"opB_o_s"      ptr val
    pokeRecordField @"opE_o_s"      ptr val
    pokeRecordField @"opGranulepos" ptr val
    pokeRecordField @"opPacketno"   ptr val



data OggSyncState =
       OggSyncState
         { ossData        :: Ptr #type unsigned char
         , ossStorage     :: #type int
         , ossFill        :: #type int
         , ossReturned    :: #type int
         , ossUnsynced    :: #type int
         , ossHeaderbytes :: #type int
         , ossBodybytes   :: #type int
         }
       deriving Show

deriveStorable #{offset ogg_sync_state, data       } "ossData"        ''OggSyncState
deriveStorable #{offset ogg_sync_state, storage    } "ossStorage"     ''OggSyncState
deriveStorable #{offset ogg_sync_state, fill       } "ossFill"        ''OggSyncState
deriveStorable #{offset ogg_sync_state, returned   } "ossReturned"    ''OggSyncState
deriveStorable #{offset ogg_sync_state, unsynced   } "ossUnsynced"    ''OggSyncState
deriveStorable #{offset ogg_sync_state, headerbytes} "ossHeaderbytes" ''OggSyncState
deriveStorable #{offset ogg_sync_state, bodybytes  } "ossBodybytes"   ''OggSyncState

instance Storable OggSyncState where
  sizeOf _    = #size      ogg_sync_state
  alignment _ = #alignment ogg_sync_state

  peek ptr =
    OggSyncState
      <$> peekField @"ossData"        ptr
      <*> peekField @"ossStorage"     ptr
      <*> peekField @"ossFill"        ptr
      <*> peekField @"ossReturned"    ptr
      <*> peekField @"ossUnsynced"    ptr
      <*> peekField @"ossHeaderbytes" ptr
      <*> peekField @"ossBodybytes"   ptr

  poke ptr val = do
    pokeRecordField @"ossData"        ptr val
    pokeRecordField @"ossStorage"     ptr val
    pokeRecordField @"ossFill"        ptr val
    pokeRecordField @"ossReturned"    ptr val
    pokeRecordField @"ossUnsynced"    ptr val
    pokeRecordField @"ossHeaderbytes" ptr val
    pokeRecordField @"ossBodybytes"   ptr val
