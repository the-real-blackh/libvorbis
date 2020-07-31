{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Container.Ogg.Raw.Types where

import           Data.Function ((&))
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

instance Storable OggpackBuffer where
  sizeOf _    = #size      oggpack_buffer
  alignment _ = #alignment oggpack_buffer

  peek ptr =
    OggpackBuffer
      <$> #{peek oggpack_buffer, endbyte} ptr
      <*> #{peek oggpack_buffer, endbit } ptr
      <*> #{peek oggpack_buffer, buffer } ptr
      <*> #{peek oggpack_buffer, ptr    } ptr
      <*> #{peek oggpack_buffer, storage} ptr

  poke ptr val = do
    #{poke oggpack_buffer, endbyte} ptr $ val & obEndbyte
    #{poke oggpack_buffer, endbit } ptr $ val & obEndbit
    #{poke oggpack_buffer, buffer } ptr $ val & obBuffer
    #{poke oggpack_buffer, ptr    } ptr $ val & obPtr
    #{poke oggpack_buffer, storage} ptr $ val & obStorage



data OggPage =
       OggPage
         { opHeader     :: Ptr #type unsigned char
         , opHeader_len :: #type long
         , opBody       :: Ptr #type unsigned char
         , opBody_len   :: #type long
         }
       deriving Show

instance Storable OggPage where
  sizeOf _    = #size      ogg_page
  alignment _ = #alignment ogg_page

  peek ptr =
    OggPage
      <$> #{peek ogg_page, header    } ptr
      <*> #{peek ogg_page, header_len} ptr
      <*> #{peek ogg_page, body      } ptr
      <*> #{peek ogg_page, body_len  } ptr

  poke ptr val = do
    #{poke ogg_page, header    } ptr $ val & opHeader
    #{poke ogg_page, header_len} ptr $ val & opHeader_len
    #{poke ogg_page, body      } ptr $ val & opBody
    #{poke ogg_page, body_len  } ptr $ val & opBody_len



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

instance Storable OggStreamState where
  sizeOf _    = #size      ogg_stream_state
  alignment _ = #alignment ogg_stream_state

  peek ptr =
    OggStreamState
      <$> #{peek ogg_stream_state, body_data      } ptr
      <*> #{peek ogg_stream_state, body_storage   } ptr
      <*> #{peek ogg_stream_state, body_fill      } ptr
      <*> #{peek ogg_stream_state, body_returned  } ptr
      <*> #{peek ogg_stream_state, lacing_vals    } ptr
      <*> #{peek ogg_stream_state, granule_vals   } ptr
      <*> #{peek ogg_stream_state, lacing_storage } ptr
      <*> #{peek ogg_stream_state, lacing_fill    } ptr
      <*> #{peek ogg_stream_state, lacing_packet  } ptr
      <*> #{peek ogg_stream_state, lacing_returned} ptr
      <*> #{peek ogg_stream_state, header         } ptr
      <*> #{peek ogg_stream_state, header_fill    } ptr
      <*> #{peek ogg_stream_state, e_o_s          } ptr
      <*> #{peek ogg_stream_state, b_o_s          } ptr
      <*> #{peek ogg_stream_state, serialno       } ptr
      <*> #{peek ogg_stream_state, pageno         } ptr
      <*> #{peek ogg_stream_state, packetno       } ptr
      <*> #{peek ogg_stream_state, granulepos     } ptr


  poke ptr val = do
    #{poke ogg_stream_state, body_data      } ptr $ val & ossBody_data
    #{poke ogg_stream_state, body_storage   } ptr $ val & ossBody_storage
    #{poke ogg_stream_state, body_fill      } ptr $ val & ossBody_fill
    #{poke ogg_stream_state, body_returned  } ptr $ val & ossBody_returned
    #{poke ogg_stream_state, lacing_vals    } ptr $ val & ossLacing_vals
    #{poke ogg_stream_state, granule_vals   } ptr $ val & ossGranule_vals
    #{poke ogg_stream_state, lacing_storage } ptr $ val & ossLacing_storage
    #{poke ogg_stream_state, lacing_fill    } ptr $ val & ossLacing_fill
    #{poke ogg_stream_state, lacing_packet  } ptr $ val & ossLacing_packet
    #{poke ogg_stream_state, lacing_returned} ptr $ val & ossLacing_returned
    #{poke ogg_stream_state, header         } ptr $ val & ossHeader
    #{poke ogg_stream_state, header_fill    } ptr $ val & ossHeader_fill
    #{poke ogg_stream_state, e_o_s          } ptr $ val & ossE_o_s
    #{poke ogg_stream_state, b_o_s          } ptr $ val & ossB_o_s
    #{poke ogg_stream_state, serialno       } ptr $ val & ossSerialno
    #{poke ogg_stream_state, pageno         } ptr $ val & ossPageno
    #{poke ogg_stream_state, packetno       } ptr $ val & ossPacketno
    #{poke ogg_stream_state, granulepos     } ptr $ val & ossGranulepos



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

instance Storable OggPacket where
  sizeOf _    = #size      ogg_packet
  alignment _ = #alignment ogg_packet

  peek ptr =
    OggPacket
      <$> #{peek ogg_packet, packet    } ptr
      <*> #{peek ogg_packet, bytes     } ptr
      <*> #{peek ogg_packet, b_o_s     } ptr
      <*> #{peek ogg_packet, e_o_s     } ptr
      <*> #{peek ogg_packet, granulepos} ptr
      <*> #{peek ogg_packet, packetno  } ptr

  poke ptr val = do
     #{poke ogg_packet, packet    } ptr $ val & opPacket
     #{poke ogg_packet, bytes     } ptr $ val & opBytes
     #{poke ogg_packet, b_o_s     } ptr $ val & opB_o_s
     #{poke ogg_packet, e_o_s     } ptr $ val & opE_o_s
     #{poke ogg_packet, granulepos} ptr $ val & opGranulepos
     #{poke ogg_packet, packetno  } ptr $ val & opPacketno



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

instance Storable OggSyncState where
  sizeOf _    = #size      ogg_sync_state
  alignment _ = #alignment ogg_sync_state

  peek ptr =
    OggSyncState
      <$> #{peek ogg_sync_state, data       } ptr
      <*> #{peek ogg_sync_state, storage    } ptr
      <*> #{peek ogg_sync_state, fill       } ptr
      <*> #{peek ogg_sync_state, returned   } ptr
      <*> #{peek ogg_sync_state, unsynced   } ptr
      <*> #{peek ogg_sync_state, headerbytes} ptr
      <*> #{peek ogg_sync_state, bodybytes  } ptr

  poke ptr val = do
    #{poke ogg_sync_state, data       } ptr $ val & ossData
    #{poke ogg_sync_state, storage    } ptr $ val & ossStorage
    #{poke ogg_sync_state, fill       } ptr $ val & ossFill
    #{poke ogg_sync_state, returned   } ptr $ val & ossReturned
    #{poke ogg_sync_state, unsynced   } ptr $ val & ossUnsynced
    #{poke ogg_sync_state, headerbytes} ptr $ val & ossHeaderbytes
    #{poke ogg_sync_state, bodybytes  } ptr $ val & ossBodybytes
