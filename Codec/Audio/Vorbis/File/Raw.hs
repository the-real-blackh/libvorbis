{-# LANGUAGE PatternSynonyms #-}

module Codec.Audio.Vorbis.File.Raw
  ( -- * Exceptions
    VorbisError (..)
    -- ** Return codes
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
    -- * Data Structutes
  , module Codec.Audio.Vorbis.File.Raw.Types
    -- ** Reexports from "Codec.Audio.Vorbis.Raw"
  , VorbisComment (..)
  , VorbisInfo (..)
    -- ** Reexports from "Codec.Container.Ogg.Raw"
  , Ogg_Int64_t
    -- * Setup/Teardown
  , module Codec.Audio.Vorbis.File.Raw.Setup
    -- * Decoding
  , module Codec.Audio.Vorbis.File.Raw.Decoding
    -- * Seeking
  , module Codec.Audio.Vorbis.File.Raw.Seeking
    -- * File Information
  , module Codec.Audio.Vorbis.File.Raw.File
  ) where

import           Codec.Audio.Vorbis.File.Raw.Decoding
import           Codec.Audio.Vorbis.File.Raw.File
import           Codec.Audio.Vorbis.File.Raw.Seeking
import           Codec.Audio.Vorbis.File.Raw.Setup
import           Codec.Audio.Vorbis.File.Raw.Types
import           Codec.Audio.Vorbis.Raw.Internal.Exception hiding (ovError, ovError')
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types (Ogg_Int64_t)
