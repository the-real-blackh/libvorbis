module Codec.Container.Ogg.Raw.Internal.Exception where

import           Control.Monad (when)
import           Control.Exception
import           Data.Int



data OggError =                                                     
       OggError
         { oeFunction :: [Char]      -- ^ Name of the function that threw an exception
         , oeCode     :: #{type int} -- ^ Function return value
         }
          
instance Show OggError where
  show (OggError name code) =
    mconcat
      [ "libogg function "
      , show name
      , " returned error code "
      , show code
      ]
       
instance Exception OggError



-- | Executes an action, checks the return code, throws an error if it's not zero.
oggError :: String -> IO #{type int} -> IO ()
oggError name action = do
  code <- action
  when (code /= 0)
    $ oggError' name code



-- | Throws an 'OggError'. This is here for when 'oggError' doesn't cut it.
oggError' :: String -> #{type int} -> IO a
oggError' name code =
  throwIO $ OggError name code
