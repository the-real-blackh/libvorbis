{-# LANGUAGE PatternSynonyms #-}

module Codec.Audio.Vorbis.Raw.Internal.Exception where

import           Control.Monad (when)
import           Control.Exception
import           Data.Int

#include "vorbis/codec.h"

data VorbisError =
       VorbisError
         { veFunction :: [Char]      -- ^ Name of the function that threw an exception
         , veCode     :: #{type int} -- ^ Function return value
         }

instance Show VorbisError where
  show (VorbisError name code) =
    mconcat
      [ "libvorbis function "
      , show name
      , " returned error code "
      , case code of
          OV_FALSE      -> show code <> " (OV_FALSE)"
          OV_EOF        -> show code <> " (OV_EOF)"
          OV_HOLE       -> show code <> " (OV_HOLE)"
          OV_EREAD      -> show code <> " (OV_EREAD)"
          OV_EFAULT     -> show code <> " (OV_EFAULT)"
          OV_EIMPL      -> show code <> " (OV_EIMPL)"
          OV_EINVAL     -> show code <> " (OV_EINVAL)"
          OV_ENOTVORBIS -> show code <> " (OV_ENOTVORBIS)"
          OV_EBADHEADER -> show code <> " (OV_EBADHEADER)"
          OV_EVERSION   -> show code <> " (OV_EVERSION)"
          OV_ENOTAUDIO  -> show code <> " (OV_ENOTAUDIO)"
          OV_EBADPACKET -> show code <> " (OV_EBADPACKET)"
          OV_EBADLINK   -> show code <> " (OV_EBADLINK)"
          OV_ENOSEEK    -> show code <> " (OV_ENOSEEK)"
          _             -> show code
      ]

instance Exception VorbisError



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



-- | Executes an action, checks the return code, throws an error if it's under zero.
ovError :: String -> IO #{type int} -> IO ()
ovError name action = do
  code <- action
  when (code < 0) $
    ovError' name code



-- | Throws a 'VorbisError'. This is here for when 'ovError' doesn't cut it.
ovError' :: String -> #{type int} -> IO a
ovError' name code =
  throwIO $ VorbisError name code
