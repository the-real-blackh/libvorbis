{-# LANGUAGE TemplateHaskell #-}

module Data.Field.Storable.TH where

import           Data.Field
import           Foreign.Ptr
import           Foreign.Storable
import           Language.Haskell.TH



deriveStorable
  :: Int    -- ^ field offset
  -> String -- ^ field name
  -> Name   -- ^ data structure name
  -> Q [Dec]
deriveStorable 0 x r =
  [d| instance Storable (Field $(litT . pure $ StrTyLit x) $(conT r)) where
        sizeOf    = foldField sizeOf
        alignment = foldField alignment

        peek ptr = mkField <$> peek (castPtr ptr)
        poke ptr = foldField $ poke (castPtr ptr)
    |]

deriveStorable offset x r =
  [d| instance Storable (Field $(litT . pure $ StrTyLit x) $(conT r)) where
        sizeOf    = foldField sizeOf
        alignment = foldField alignment

        peek ptr = mkField <$> peekByteOff ptr offset
        poke ptr = foldField $ pokeByteOff ptr offset
    |]



deriveStorable1
  :: Int    -- ^ field offset
  -> String -- ^ field name
  -> Name   -- ^ data structure name
  -> Q [Dec]
deriveStorable1 0 x r =
  [d| instance Storable (Field $(litT . pure $ StrTyLit x) $(do a <- newName "a"
                                                                appT (conT r) $ varT a
                                                            )) where
        sizeOf    = foldField sizeOf
        alignment = foldField alignment

        peek ptr = mkField <$> peek (castPtr ptr)
        poke ptr = foldField $ poke (castPtr ptr)
    |]

deriveStorable1 offset x r =
  [d| instance Storable (Field $(litT . pure $ StrTyLit x) $(do a <- newName "a"
                                                                appT (conT r) $ varT a
                                                            )) where
        sizeOf    = foldField sizeOf
        alignment = foldField alignment

        peek ptr = mkField <$> peekByteOff ptr offset
        poke ptr = foldField $ pokeByteOff ptr offset
    |]
