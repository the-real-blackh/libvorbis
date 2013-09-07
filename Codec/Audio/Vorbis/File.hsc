{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables,
             DeriveDataTypeable #-}
module Codec.Audio.Vorbis.File (
        File,
        openFile,
        openCallbacks,
        withFile,
        withCallbacks,
        info,
        Info(..),
        Channels(..),
        read,
        close,
        Endianness(..),
        WordSize(..),
        Signedness(..),
        getSystemEndianness
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Control.Exception
import Data.Int
import Data.Typeable
import Foreign hiding (new)
import Foreign.C
import Prelude hiding (read)
import System.IO (SeekMode(..), hClose, hSeek, hTell, IOMode(..))
import qualified System.IO as IO
import System.Endian

#include <vorbis/vorbisfile.h>
#include <stdlib.h>

type ReadFunc = Ptr Word8 -> CSize -> CSize -> Ptr () -> IO CSize
type SeekFunc = Ptr () -> Int64 -> CInt -> IO CInt
type CloseFunc = Ptr () -> IO CInt
type TellFunc  = Ptr () -> IO CLong

foreign import ccall safe "new_OggVorbis_File" new_OggVorbis_File
    :: FunPtr ReadFunc -> FunPtr SeekFunc -> FunPtr CloseFunc -> FunPtr TellFunc -> Ptr CInt -> IO (Ptr File_struct)

foreign import ccall "free_OggVorbis_File" free_OggVorbis_File
    :: Ptr File_struct -> IO ()

foreign import ccall "wrapper" mkReadFunc
    :: ReadFunc -> IO (FunPtr ReadFunc)

foreign import ccall "wrapper" mkSeekFunc
    :: SeekFunc -> IO (FunPtr SeekFunc)

foreign import ccall "wrapper" mkCloseFunc
    :: CloseFunc -> IO (FunPtr CloseFunc)

foreign import ccall "wrapper" mkTellFunc
    :: TellFunc -> IO (FunPtr TellFunc)

foreign import ccall safe "info_OggVorbis_File" info_OggVorbis_File
    :: Ptr File_struct -> Ptr CInt -> Ptr CInt -> Ptr CLong -> IO CInt

foreign import ccall safe "ov_read" ov_read
    :: Ptr File_struct -> Ptr CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO CLong

data File_struct
data File = File (Ptr File_struct) (FunPtr ReadFunc) (FunPtr CloseFunc) (FunPtr SeekFunc) (FunPtr TellFunc)

data OggVorbisStatus =
      OV_EREAD
    | OV_EFAULT
    | OV_EIMPL
    | OV_EINVAL
    | OV_ENOTVORBIS
    | OV_EBADHEADER
    | OV_EVERSION
    | OV_ENOTAUDIO
    | OV_EBADPACKET
    | OV_EBADLINK
    | OV_ENOSEEK
    | OV_BADINFO         -- Ours
    deriving (Eq, Show)

data OggVorbisException = OggVorbisException OggVorbisStatus
    deriving (Eq, Show, Typeable)

instance Exception OggVorbisException where

throwVorbisError :: CInt -> IO a
throwVorbisError err = do
    let status = case err of
            (#const OV_EREAD)      -> OV_EREAD
            (#const OV_EFAULT)     -> OV_EFAULT
            (#const OV_EIMPL)      -> OV_EIMPL
            (#const OV_EINVAL)     -> OV_EINVAL
            (#const OV_ENOTVORBIS) -> OV_ENOTVORBIS
            (#const OV_EBADHEADER) -> OV_EBADHEADER
            (#const OV_EVERSION)   -> OV_EVERSION
            (#const OV_ENOTAUDIO)  -> OV_ENOTAUDIO
            (#const OV_EBADPACKET) -> OV_EBADPACKET
            (#const OV_EBADLINK)   -> OV_EBADLINK
            (#const OV_ENOSEEK)    -> OV_ENOSEEK
            _                      -> error $ "Codec.Audio.Vorbis.File.throwVorbisError: bad error: "++show err
    throwIO $ OggVorbisException status

openFile :: FilePath -> IO File
openFile path = do
    h <- IO.openFile path ReadMode
    let readFunc size nmemb = B.hGet h (size * nmemb)
        closeFunc = hClose h
        seekFunc = hSeek h
        tellFunc = hTell h
    openCallbacks readFunc closeFunc (Just (seekFunc, tellFunc))

openCallbacks :: (Int -> Int -> IO ByteString)    -- ^ Read function, taking size and nmemb
              -> IO ()                            -- ^ Close function
              -> Maybe (SeekMode -> Integer -> IO (), IO Integer)  -- ^ Seek and tell functions
              -> IO File
openCallbacks readFunc closeFunc mSeekTell = do
    c_readFunc <- mkReadFunc $ \buf size nmemb _ -> do
        r <- readFunc (fromIntegral size) (fromIntegral nmemb)
        B.unsafeUseAsCStringLen r $ \(rbuf, rlen) -> B.memcpy (castPtr buf) (castPtr rbuf) rlen
        return $ fromIntegral (B.length r) `div` size
      `catch` \(_ :: IOException) ->
        return (-1)
    c_closeFunc <- mkCloseFunc $ \_ -> do
        closeFunc
        return 0
      `catch` \(_ :: IOException) ->
        return (-1)
    (c_seekFunc, c_tellFunc) <- case mSeekTell of
        Just (seekFunc, tellFunc) -> do
            c_seekFunc <- mkSeekFunc $ \_ offset whence -> do
                let mode = case whence of
                        (#const SEEK_SET) -> AbsoluteSeek
                        (#const SEEK_CUR) -> RelativeSeek
                        (#const SEEK_END) -> SeekFromEnd
                        _                 -> error $ "Codec.Audio.Vorbis.File.new: bad whence in seek: "++show whence
                seekFunc mode (fromIntegral offset)
                return 0
              `catch` \(_ :: IOException) ->
                return (-1)
            c_tellFunc <- mkTellFunc $ \_ -> do
                fromIntegral <$> tellFunc
              `catch` \(_ :: IOException) ->
                return (-1)
            return (c_seekFunc, c_tellFunc)
        Nothing -> return (nullFunPtr, nullFunPtr)
    alloca $ \p_error -> do
        f <- new_OggVorbis_File c_readFunc c_seekFunc c_closeFunc c_tellFunc p_error
        if f /= nullPtr then
            return $ File f c_readFunc c_closeFunc c_seekFunc c_tellFunc
          else do
            freeFunPtrs c_readFunc c_closeFunc c_seekFunc c_tellFunc
            err <- peek p_error
            throwVorbisError err

close :: File -> IO ()
close (File c_f c_readFunc c_closeFunc c_seekFunc c_tellFunc) = do
    free_OggVorbis_File c_f
    freeFunPtrs c_readFunc c_closeFunc c_seekFunc c_tellFunc

freeFunPtrs :: FunPtr ReadFunc -> FunPtr CloseFunc -> FunPtr SeekFunc -> FunPtr TellFunc -> IO ()
freeFunPtrs c_readFunc c_seekFunc c_closeFunc c_tellFunc = do
    when (c_seekFunc /= nullFunPtr) $ freeHaskellFunPtr c_seekFunc
    when (c_tellFunc /= nullFunPtr) $ freeHaskellFunPtr c_tellFunc
    freeHaskellFunPtr c_readFunc
    freeHaskellFunPtr c_closeFunc

-- | Open the file using the loan pattern. It calls 'close' for you, so don't call it
-- explicitly.
withFile :: FilePath -> (File -> IO a) -> IO a
withFile path code = bracket (openFile path) close code

-- | Open the stream using the loan pattern. It calls 'close' for you, so don't call it
-- explicitly.
withCallbacks :: (Int -> Int -> IO ByteString)    -- ^ Read function, taking size and nmemb
              -> IO ()                            -- ^ Close function
              -> Maybe (SeekMode -> Integer -> IO (), IO Integer)  -- ^ Seek and tell functions
              -> (File -> IO a)
              -> IO a
withCallbacks r c mST code = bracket (openCallbacks r c mST) close code

data Info = Info {
        inVersion  :: Int,
        inChannels :: Channels,
        inRate     :: Int
    }
    deriving Show

data Channels = Mono | Stereo deriving (Eq, Ord, Show)

info :: File -> IO Info
info (File c_f _ _ _ _) =
    alloca $ \p_version ->
    alloca $ \p_channels ->
    alloca $ \p_rate -> do
        ret <- info_OggVorbis_File c_f p_version p_channels p_rate
        if ret == 0 then
            Info <$> (fromIntegral <$> peek p_version)
                 <*> (unmarshallChannels <$> peek p_channels)
                 <*> (fromIntegral <$> peek p_rate)
          else do
            throwIO $ OggVorbisException OV_BADINFO
  where
    unmarshallChannels 1 = Mono
    unmarshallChannels 2 = Stereo
    unmarshallChannels c = error $ "Codec.Audio.Vorbis.info: can't unmarshall no. of channels: "++show c

data Signedness = Signed | Unsigned deriving (Eq, Ord, Show)
data WordSize = EightBit | SixteenBit deriving (Eq, Ord, Show)

-- | Read data from the file. Returns the data block and the number of the current
-- logical bitstream. 'Nothing' for end of file.
read :: File
     -> Int          -- ^ Maximum bytes to read (will typically return less than this)
     -> Endianness   -- ^ How to encode the samples as bytes
     -> WordSize     -- ^ Desired word size
     -> Signedness   -- ^ Whether you want signed or unsigned values
     -> IO (Maybe (ByteString, Int)) 
read (File f _ _ _ _) bytes endianness wordsize signedness =
    allocaBytes bytes $ \buf ->
    alloca $ \p_bitstream -> go buf p_bitstream
  where
    go buf p_bitstream = do
        ret <- ov_read f buf (fromIntegral bytes) c_endianness c_wordsize c_signedness p_bitstream
        case ret of
            -- Deal with hole by just continuing to read. We might want to improve this
            -- in future.
            (#const OV_HOLE) -> go buf p_bitstream
            0 -> return Nothing
            n | n > 0 -> do
                bs <- B.create (fromIntegral n) (\bs_buf -> B.memcpy bs_buf (castPtr buf) (fromIntegral n))
                bitstream <- fromIntegral <$> peek p_bitstream
                return $ Just (bs, bitstream)
            _ -> throwVorbisError (fromIntegral ret)
    c_endianness = case endianness of
        LittleEndian -> 0
        BigEndian    -> 1
    c_wordsize = case wordsize of
        EightBit     -> 1
        SixteenBit   -> 2
    c_signedness = case signedness of
        Unsigned     -> 0
        Signed       -> 1

