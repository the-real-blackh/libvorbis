{-# LANGUAGE DataKinds
           , DuplicateRecordFields
           , FlexibleInstances
           , ForeignFunctionInterface
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE TypeApplications #-}

module Libogg.Types where

import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset
import           GHC.Records

#include "ogg/ogg.h"

type Ogg_Int64_t = #type ogg_int64_t



data {-# CTYPE "ogg/ogg.h" "oggpack_buffer" #-} OggpackBuffer =
       OggpackBuffer
         { endbyte :: #type long
         , endbit  :: #type int
         , buffer  :: Ptr #type unsigned char
         , ptr     :: Ptr #type unsigned char
         , storage :: #type long
         }
       deriving Show

instance Offset "endbyte" OggpackBuffer where rawOffset = #{offset oggpack_buffer, endbyte}
instance Offset "endbit"  OggpackBuffer where rawOffset = #{offset oggpack_buffer, endbit }
instance Offset "buffer"  OggpackBuffer where rawOffset = #{offset oggpack_buffer, buffer }
instance Offset "ptr"     OggpackBuffer where rawOffset = #{offset oggpack_buffer, ptr    }
instance Offset "storage" OggpackBuffer where rawOffset = #{offset oggpack_buffer, storage}

instance Storable OggpackBuffer where
  sizeOf _    = #size      oggpack_buffer
  alignment _ = #alignment oggpack_buffer

  peek ptr_ =
    OggpackBuffer
      <$> peek (offset @"endbyte" ptr_)
      <*> peek (offset @"endbit"  ptr_)
      <*> peek (offset @"buffer"  ptr_)
      <*> peek (offset @"ptr"     ptr_)
      <*> peek (offset @"storage" ptr_)

  poke ptr_ val = do
    pokeField @"endbyte" ptr_ val
    pokeField @"endbit"  ptr_ val
    pokeField @"buffer"  ptr_ val
    pokeField @"ptr"     ptr_ val
    pokeField @"storage" ptr_ val



data {-# CTYPE "ogg/ogg.h" "ogg_page" #-} OggPage =
       OggPage
         { header     :: Ptr #type unsigned char
         , header_len :: #type long
         , body       :: Ptr #type unsigned char
         , body_len   :: #type long
         }
       deriving Show

instance Offset "header"     OggPage where rawOffset = #{offset ogg_page, header    }
instance Offset "header_len" OggPage where rawOffset = #{offset ogg_page, header_len}
instance Offset "body"       OggPage where rawOffset = #{offset ogg_page, body      }
instance Offset "body_len"   OggPage where rawOffset = #{offset ogg_page, body_len  }

instance Storable OggPage where
  sizeOf _    = #size      ogg_page
  alignment _ = #alignment ogg_page

  peek ptr_ =
    OggPage
      <$> peek (offset @"header"     ptr_)
      <*> peek (offset @"header_len" ptr_)
      <*> peek (offset @"body"       ptr_)
      <*> peek (offset @"body_len"   ptr_)

  poke ptr_ val = do
    pokeField @"header"     ptr_ val
    pokeField @"header_len" ptr_ val
    pokeField @"body"       ptr_ val
    pokeField @"body_len"   ptr_ val



data{-# CTYPE "ogg/ogg.h" "ogg_stream_state" #-} OggStreamState =
       OggStreamState
         { body_data       :: Ptr #type unsigned char
         , body_storage    :: #type long
         , body_fill       :: #type long
         , body_returned   :: #type long
         , lacing_vals     :: Ptr #type int
         , granule_vals    :: Ptr Ogg_Int64_t
         , lacing_storage  :: #type long
         , lacing_fill     :: #type long
         , lacing_packet   :: #type long
         , lacing_returned :: #type long
         , header          :: Ptr #type unsigned char
         , header_fill     :: #type int
         , e_o_s           :: #type int
         , b_o_s           :: #type int
         , serialno        :: #type long
         , pageno          :: #type long
         , packetno        :: Ogg_Int64_t
         , granulepos      :: Ogg_Int64_t
         }
       deriving Show

instance Offset "body_data"       OggStreamState where rawOffset = #{offset ogg_stream_state, body_data      }
instance Offset "body_storage"    OggStreamState where rawOffset = #{offset ogg_stream_state, body_storage   }
instance Offset "body_fill"       OggStreamState where rawOffset = #{offset ogg_stream_state, body_fill      }
instance Offset "body_returned"   OggStreamState where rawOffset = #{offset ogg_stream_state, body_returned  }
instance Offset "lacing_vals"     OggStreamState where rawOffset = #{offset ogg_stream_state, lacing_vals    }
instance Offset "granule_vals"    OggStreamState where rawOffset = #{offset ogg_stream_state, granule_vals   }
instance Offset "lacing_storage"  OggStreamState where rawOffset = #{offset ogg_stream_state, lacing_storage }
instance Offset "lacing_fill"     OggStreamState where rawOffset = #{offset ogg_stream_state, lacing_fill    }
instance Offset "lacing_packet"   OggStreamState where rawOffset = #{offset ogg_stream_state, lacing_packet  }
instance Offset "lacing_returned" OggStreamState where rawOffset = #{offset ogg_stream_state, lacing_returned}
instance Offset "header"          OggStreamState where rawOffset = #{offset ogg_stream_state, header         }
instance Offset "header_fill"     OggStreamState where rawOffset = #{offset ogg_stream_state, header_fill    }
instance Offset "e_o_s"           OggStreamState where rawOffset = #{offset ogg_stream_state, e_o_s          }
instance Offset "b_o_s"           OggStreamState where rawOffset = #{offset ogg_stream_state, b_o_s          }
instance Offset "serialno"        OggStreamState where rawOffset = #{offset ogg_stream_state, serialno       }
instance Offset "pageno"          OggStreamState where rawOffset = #{offset ogg_stream_state, pageno         }
instance Offset "packetno"        OggStreamState where rawOffset = #{offset ogg_stream_state, packetno       }
instance Offset "granulepos"      OggStreamState where rawOffset = #{offset ogg_stream_state, granulepos     }

instance Storable OggStreamState where
  sizeOf _    = #size      ogg_stream_state
  alignment _ = #alignment ogg_stream_state

  peek ptr_ =
    OggStreamState
      <$> peek (offset @"body_data"       ptr_)
      <*> peek (offset @"body_storage"    ptr_)
      <*> peek (offset @"body_fill"       ptr_)
      <*> peek (offset @"body_returned"   ptr_)
      <*> peek (offset @"lacing_vals"     ptr_)
      <*> peek (offset @"granule_vals"    ptr_)
      <*> peek (offset @"lacing_storage"  ptr_)
      <*> peek (offset @"lacing_fill"     ptr_)
      <*> peek (offset @"lacing_packet"   ptr_)
      <*> peek (offset @"lacing_returned" ptr_)
      <*> peek (offset @"header"          ptr_)
      <*> peek (offset @"header_fill"     ptr_)
      <*> peek (offset @"e_o_s"           ptr_)
      <*> peek (offset @"b_o_s"           ptr_)
      <*> peek (offset @"serialno"        ptr_)
      <*> peek (offset @"pageno"          ptr_)
      <*> peek (offset @"packetno"        ptr_)
      <*> peek (offset @"granulepos"      ptr_)

  poke ptr_ val = do
    pokeField @"body_data"       ptr_ val
    pokeField @"body_storage"    ptr_ val
    pokeField @"body_fill"       ptr_ val
    pokeField @"body_returned"   ptr_ val
    pokeField @"lacing_vals"     ptr_ val
    pokeField @"granule_vals"    ptr_ val
    pokeField @"lacing_storage"  ptr_ val
    pokeField @"lacing_fill"     ptr_ val
    pokeField @"lacing_packet"   ptr_ val
    pokeField @"lacing_returned" ptr_ val
    pokeField @"header"          ptr_ val
    pokeField @"header_fill"     ptr_ val
    pokeField @"e_o_s"           ptr_ val
    pokeField @"b_o_s"           ptr_ val
    pokeField @"serialno"        ptr_ val
    pokeField @"pageno"          ptr_ val
    pokeField @"packetno"        ptr_ val
    pokeField @"granulepos"      ptr_ val



data {-# CTYPE "ogg/ogg.h" "ogg_packet" #-} OggPacket =
       OggPacket
         { packet     :: Ptr #type unsigned char
         , bytes      :: #type long
         , b_o_s      :: #type long
         , e_o_s      :: #type long
         , granulepos :: Ogg_Int64_t
         , packetno   :: Ogg_Int64_t
         }
       deriving Show

instance Offset "packet"     OggPacket where rawOffset = #{offset ogg_packet, packet    }
instance Offset "bytes"      OggPacket where rawOffset = #{offset ogg_packet, bytes     }
instance Offset "b_o_s"      OggPacket where rawOffset = #{offset ogg_packet, b_o_s     }
instance Offset "e_o_s"      OggPacket where rawOffset = #{offset ogg_packet, e_o_s     }
instance Offset "granulepos" OggPacket where rawOffset = #{offset ogg_packet, granulepos}
instance Offset "packetno"   OggPacket where rawOffset = #{offset ogg_packet, packetno  }

instance Storable OggPacket where
  sizeOf _    = #size      ogg_packet
  alignment _ = #alignment ogg_packet

  peek ptr_ =
    OggPacket
      <$> peek (offset @"packet"     ptr_)
      <*> peek (offset @"bytes"      ptr_)
      <*> peek (offset @"b_o_s"      ptr_)
      <*> peek (offset @"e_o_s"      ptr_)
      <*> peek (offset @"granulepos" ptr_)
      <*> peek (offset @"packetno"   ptr_)

  poke ptr_ val = do
    pokeField @"packet"     ptr_ val
    pokeField @"bytes"      ptr_ val
    pokeField @"b_o_s"      ptr_ val
    pokeField @"e_o_s"      ptr_ val
    pokeField @"granulepos" ptr_ val
    pokeField @"packetno"   ptr_ val



data {-# CTYPE "ogg/ogg.h" "ogg_sync_state" #-} OggSyncState =
       OggSyncState
         { data_       :: Ptr #type unsigned char
         , storage     :: #type int
         , fill        :: #type int
         , returned    :: #type int
         , unsynced    :: #type int
         , headerbytes :: #type int
         , bodybytes   :: #type int
         }
       deriving Show

instance Offset "data_"       OggSyncState where rawOffset = #{offset ogg_sync_state, data       }
instance Offset "storage"     OggSyncState where rawOffset = #{offset ogg_sync_state, storage    }
instance Offset "fill"        OggSyncState where rawOffset = #{offset ogg_sync_state, fill       }
instance Offset "returned"    OggSyncState where rawOffset = #{offset ogg_sync_state, returned   }
instance Offset "unsynced"    OggSyncState where rawOffset = #{offset ogg_sync_state, unsynced   }
instance Offset "headerbytes" OggSyncState where rawOffset = #{offset ogg_sync_state, headerbytes}
instance Offset "bodybytes"   OggSyncState where rawOffset = #{offset ogg_sync_state, bodybytes  }

instance Offset "data" OggSyncState where
  rawOffset = rawOffset @"data_" @OggSyncState

instance HasField "data" OggSyncState (Ptr #{type unsigned char}) where
  getField = getField @"data_"

instance Storable OggSyncState where
  sizeOf _    = #size      ogg_sync_state
  alignment _ = #alignment ogg_sync_state

  peek ptr_ =
    OggSyncState
      <$> peek (offset @"data"        ptr_)
      <*> peek (offset @"storage"     ptr_)
      <*> peek (offset @"fill"        ptr_)
      <*> peek (offset @"returned"    ptr_)
      <*> peek (offset @"unsynced"    ptr_)
      <*> peek (offset @"headerbytes" ptr_)
      <*> peek (offset @"bodybytes"   ptr_)

  poke ptr_ val = do
    pokeField @"data"        ptr_ val
    pokeField @"storage"     ptr_ val
    pokeField @"fill"        ptr_ val
    pokeField @"returned"    ptr_ val
    pokeField @"unsynced"    ptr_ val
    pokeField @"headerbytes" ptr_ val
    pokeField @"bodybytes"   ptr_ val
