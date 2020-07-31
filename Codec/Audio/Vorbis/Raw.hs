{-# LANGUAGE PatternSynonyms #-}

module Codec.Audio.Vorbis.Raw
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
    -- * Data Structures
  , module Codec.Audio.Vorbis.Raw.Types
    -- ** Reexports from "Codec.Container.Ogg.Raw"
  , Ogg_Int64_t
    -- * Functions used by both decode and encode
  , module Codec.Audio.Vorbis.Raw.Both
    -- * Decoding
  , module Codec.Audio.Vorbis.Raw.Decoding
    -- * Encoding
  , module Codec.Audio.Vorbis.Raw.Encoding
    -- * Metadata
  , module Codec.Audio.Vorbis.Raw.Metadata
  ) where

import           Codec.Audio.Vorbis.Raw.Both
import           Codec.Audio.Vorbis.Raw.Decoding
import           Codec.Audio.Vorbis.Raw.Encoding
import           Codec.Audio.Vorbis.Raw.Internal.Exception hiding (ovError, ovError')
import           Codec.Audio.Vorbis.Raw.Metadata
import           Codec.Audio.Vorbis.Raw.Types
import           Codec.Container.Ogg.Raw.Types (Ogg_Int64_t)
