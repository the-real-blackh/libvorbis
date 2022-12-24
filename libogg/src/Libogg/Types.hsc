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

type Ogg_int64_t = #type ogg_int64_t



data {-# CTYPE "ogg/ogg.h" "oggpack_buffer" #-} Oggpack_buffer =
       Oggpack_buffer
         { endbyte :: #type long
         , endbit  :: #type int
         , buffer  :: Ptr #type unsigned char
         , ptr     :: Ptr #type unsigned char
         , storage :: #type long
         }
       deriving Show

instance Offset "endbyte" Oggpack_buffer where rawOffset = #{offset oggpack_buffer, endbyte}
instance Offset "endbit"  Oggpack_buffer where rawOffset = #{offset oggpack_buffer, endbit }
instance Offset "buffer"  Oggpack_buffer where rawOffset = #{offset oggpack_buffer, buffer }
instance Offset "ptr"     Oggpack_buffer where rawOffset = #{offset oggpack_buffer, ptr    }
instance Offset "storage" Oggpack_buffer where rawOffset = #{offset oggpack_buffer, storage}

instance Storable Oggpack_buffer where
  sizeOf _    = #size      oggpack_buffer
  alignment _ = #alignment oggpack_buffer

  peek ptr_ =
    Oggpack_buffer
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



data {-# CTYPE "ogg/ogg.h" "ogg_page" #-} Ogg_page =
       Ogg_page
         { header     :: Ptr #type unsigned char
         , header_len :: #type long
         , body       :: Ptr #type unsigned char
         , body_len   :: #type long
         }
       deriving Show

instance Offset "header"     Ogg_page where rawOffset = #{offset ogg_page, header    }
instance Offset "header_len" Ogg_page where rawOffset = #{offset ogg_page, header_len}
instance Offset "body"       Ogg_page where rawOffset = #{offset ogg_page, body      }
instance Offset "body_len"   Ogg_page where rawOffset = #{offset ogg_page, body_len  }

instance Storable Ogg_page where
  sizeOf _    = #size      ogg_page
  alignment _ = #alignment ogg_page

  peek ptr_ =
    Ogg_page
      <$> peek (offset @"header"     ptr_)
      <*> peek (offset @"header_len" ptr_)
      <*> peek (offset @"body"       ptr_)
      <*> peek (offset @"body_len"   ptr_)

  poke ptr_ val = do
    pokeField @"header"     ptr_ val
    pokeField @"header_len" ptr_ val
    pokeField @"body"       ptr_ val
    pokeField @"body_len"   ptr_ val



data{-# CTYPE "ogg/ogg.h" "ogg_stream_state" #-} Ogg_stream_state =
       Ogg_stream_state
         { body_data       :: Ptr #type unsigned char
         , body_storage    :: #type long
         , body_fill       :: #type long
         , body_returned   :: #type long
         , lacing_vals     :: Ptr #type int
         , granule_vals    :: Ptr Ogg_int64_t
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
         , packetno        :: Ogg_int64_t
         , granulepos      :: Ogg_int64_t
         }
       deriving Show

instance Offset "body_data"       Ogg_stream_state where rawOffset = #{offset ogg_stream_state, body_data      }
instance Offset "body_storage"    Ogg_stream_state where rawOffset = #{offset ogg_stream_state, body_storage   }
instance Offset "body_fill"       Ogg_stream_state where rawOffset = #{offset ogg_stream_state, body_fill      }
instance Offset "body_returned"   Ogg_stream_state where rawOffset = #{offset ogg_stream_state, body_returned  }
instance Offset "lacing_vals"     Ogg_stream_state where rawOffset = #{offset ogg_stream_state, lacing_vals    }
instance Offset "granule_vals"    Ogg_stream_state where rawOffset = #{offset ogg_stream_state, granule_vals   }
instance Offset "lacing_storage"  Ogg_stream_state where rawOffset = #{offset ogg_stream_state, lacing_storage }
instance Offset "lacing_fill"     Ogg_stream_state where rawOffset = #{offset ogg_stream_state, lacing_fill    }
instance Offset "lacing_packet"   Ogg_stream_state where rawOffset = #{offset ogg_stream_state, lacing_packet  }
instance Offset "lacing_returned" Ogg_stream_state where rawOffset = #{offset ogg_stream_state, lacing_returned}
instance Offset "header"          Ogg_stream_state where rawOffset = #{offset ogg_stream_state, header         }
instance Offset "header_fill"     Ogg_stream_state where rawOffset = #{offset ogg_stream_state, header_fill    }
instance Offset "e_o_s"           Ogg_stream_state where rawOffset = #{offset ogg_stream_state, e_o_s          }
instance Offset "b_o_s"           Ogg_stream_state where rawOffset = #{offset ogg_stream_state, b_o_s          }
instance Offset "serialno"        Ogg_stream_state where rawOffset = #{offset ogg_stream_state, serialno       }
instance Offset "pageno"          Ogg_stream_state where rawOffset = #{offset ogg_stream_state, pageno         }
instance Offset "packetno"        Ogg_stream_state where rawOffset = #{offset ogg_stream_state, packetno       }
instance Offset "granulepos"      Ogg_stream_state where rawOffset = #{offset ogg_stream_state, granulepos     }

instance Storable Ogg_stream_state where
  sizeOf _    = #size      ogg_stream_state
  alignment _ = #alignment ogg_stream_state

  peek ptr_ =
    Ogg_stream_state
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



data {-# CTYPE "ogg/ogg.h" "ogg_packet" #-} Ogg_packet =
       Ogg_packet
         { packet     :: Ptr #type unsigned char
         , bytes      :: #type long
         , b_o_s      :: #type long
         , e_o_s      :: #type long
         , granulepos :: Ogg_int64_t
         , packetno   :: Ogg_int64_t
         }
       deriving Show

instance Offset "packet"     Ogg_packet where rawOffset = #{offset ogg_packet, packet    }
instance Offset "bytes"      Ogg_packet where rawOffset = #{offset ogg_packet, bytes     }
instance Offset "b_o_s"      Ogg_packet where rawOffset = #{offset ogg_packet, b_o_s     }
instance Offset "e_o_s"      Ogg_packet where rawOffset = #{offset ogg_packet, e_o_s     }
instance Offset "granulepos" Ogg_packet where rawOffset = #{offset ogg_packet, granulepos}
instance Offset "packetno"   Ogg_packet where rawOffset = #{offset ogg_packet, packetno  }

instance Storable Ogg_packet where
  sizeOf _    = #size      ogg_packet
  alignment _ = #alignment ogg_packet

  peek ptr_ =
    Ogg_packet
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



data {-# CTYPE "ogg/ogg.h" "ogg_sync_state" #-} Ogg_sync_state =
       Ogg_sync_state
         { data_       :: Ptr #type unsigned char
         , storage     :: #type int
         , fill        :: #type int
         , returned    :: #type int
         , unsynced    :: #type int
         , headerbytes :: #type int
         , bodybytes   :: #type int
         }
       deriving Show

instance Offset "data_"       Ogg_sync_state where rawOffset = #{offset ogg_sync_state, data       }
instance Offset "storage"     Ogg_sync_state where rawOffset = #{offset ogg_sync_state, storage    }
instance Offset "fill"        Ogg_sync_state where rawOffset = #{offset ogg_sync_state, fill       }
instance Offset "returned"    Ogg_sync_state where rawOffset = #{offset ogg_sync_state, returned   }
instance Offset "unsynced"    Ogg_sync_state where rawOffset = #{offset ogg_sync_state, unsynced   }
instance Offset "headerbytes" Ogg_sync_state where rawOffset = #{offset ogg_sync_state, headerbytes}
instance Offset "bodybytes"   Ogg_sync_state where rawOffset = #{offset ogg_sync_state, bodybytes  }

instance Offset "data" Ogg_sync_state where
  rawOffset = rawOffset @"data_" @Ogg_sync_state

instance HasField "data" Ogg_sync_state (Ptr #{type unsigned char}) where
  getField = getField @"data_"

instance Storable Ogg_sync_state where
  sizeOf _    = #size      ogg_sync_state
  alignment _ = #alignment ogg_sync_state

  peek ptr_ =
    Ogg_sync_state
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
