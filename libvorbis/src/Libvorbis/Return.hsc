{-# LANGUAGE CApiFFI
           , CPP
           , PatternSynonyms #-}

module Libvorbis.Return where

#include "vorbis/codec.h"

pattern OV_FALSE
      , OV_EOF
      , OV_HOLE
      , OV_EREAD
      , OV_EFAULT
      , OV_EIMPL
      , OV_EINVAL
      , OV_ENOTVORBIS
      , OV_EBADHEADER
      , OV_EVERSION
      , OV_ENOTAUDIO
      , OV_EBADPACKET
      , OV_EBADLINK
      , OV_ENOSEEK
     :: (Eq a, Num a) => a
pattern OV_FALSE      = #const OV_FALSE
pattern OV_EOF        = #const OV_EOF
pattern OV_HOLE       = #const OV_HOLE
pattern OV_EREAD      = #const OV_EREAD
pattern OV_EFAULT     = #const OV_EFAULT
pattern OV_EIMPL      = #const OV_EIMPL
pattern OV_EINVAL     = #const OV_EINVAL
pattern OV_ENOTVORBIS = #const OV_ENOTVORBIS
pattern OV_EBADHEADER = #const OV_EBADHEADER
pattern OV_EVERSION   = #const OV_EVERSION
pattern OV_ENOTAUDIO  = #const OV_ENOTAUDIO
pattern OV_EBADPACKET = #const OV_EBADPACKET
pattern OV_EBADLINK   = #const OV_EBADLINK
pattern OV_ENOSEEK    = #const OV_ENOSEEK
