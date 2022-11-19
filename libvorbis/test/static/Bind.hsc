{-# LANGUAGE FlexibleInstances
           , FunctionalDependencies
           , UndecidableInstances #-}

module Bind where

import           Data.Int
import           Data.Word
import           Foreign.Ptr



class Def a where
  def :: a

instance Def Float where
  def = 0

instance Def Double where
  def = 0

instance Def Word8 where
  def = 0

instance Def Word16 where
  def = 0

instance Def Word32 where
  def = 0

instance Def Word64 where
  def = 0

instance Def Int8 where
  def = 0

instance Def Int16 where
  def = 0

instance Def Int32 where
  def = 0

instance Def Int64 where
  def = 0

instance Def (Ptr a) where
  def = nullPtr

instance Def (FunPtr a) where
  def = nullFunPtr



class Bind a z | a -> z where
  bind :: a -> IO z

instance Bind (IO z) z where
  bind = id

instance Def a => Bind (a -> IO z) z where
  bind = ($ def)

instance (Def a, Bind (b -> c) z) => Bind (a -> b -> c) z where
  bind = bind . ($ def)
