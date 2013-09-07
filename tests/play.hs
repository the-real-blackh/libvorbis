import Control.Monad
import qualified Data.ByteString as B
import qualified Codec.Audio.Vorbis.File as V
import Sound.Tomato.Speakers  -- cabal install tomato-rubato-openal
import Data.Bits
import Data.Int
import Data.Word
import System.Environment
import System.IO

main = do
    args <- getArgs
    case args of
        [path] -> do
            V.withFile path $ \f -> do
                info <- V.info f
                putStrLn $ "channels: "++show (V.inChannels info)
                putStrLn $ "sampling rate: "++show (V.inRate info)
                putStrLn ""
                putStrLn $ "Note: This player is extremely basic. It handles stereo by adding the two"
                putStrLn $ "channels together and putting them out as one channel, so stereo files will"
                putStrLn $ "come out as mono."
                withSpeakers (fromIntegral (V.inRate info)) blkSize $ \spk ->
                    loop f spk (V.inChannels info)
        _ -> hPutStrLn stderr $ "please specify an ogg vorbis file"
  where
    blkSize = 512
    intoPairs f (a:b:xs) = f a b:intoPairs f xs
    intoPairs _ _        = []
    decode :: Word8 -> Word8 -> Int16
    decode b1 b2 = fromIntegral b1 `shiftL` 8 .|. fromIntegral b2
    loop f spk channels = do
        mBLK <- V.read f blkSize V.BigEndian V.SixteenBit V.Signed
        case mBLK of
            Just (blk, i) -> do
                let samples = map (\i -> fromIntegral i / 32768) (intoPairs decode . B.unpack $ blk)
                    samples' = case channels of
                        V.Mono -> samples
                        V.Stereo -> map (0.5*) . intoPairs (+) $ samples
                playSamples spk samples'
                --print samples
                loop f spk channels
            Nothing       -> return ()

