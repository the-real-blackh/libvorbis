{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , FlexibleContexts
           , KindSignatures
           , PatternSynonyms
           , ScopedTypeVariables
           , TypeApplications
           , UndecidableInstances
           , ViewPatterns #-}

module Data.Field.Internal where

import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Exts
import           GHC.Records
import           GHC.TypeLits
import           Unsafe.Coerce



-- | Wrapper around fields, format shamelessly stolen off of the glorious
--   [some](https://hackage.haskell.org/package/some/docs/Data-Some-Newtype.html) package.
newtype Field (x :: Symbol) r = UnsafeField { fromUnsafeField :: Any }

instance (HasField x r a, Show a) => Show (Field x r) where
  show = show . fromField

pattern Field :: HasField x r a => a -> Field x r
pattern Field a <- UnsafeField (unsafeCoerce -> a)
  where
    Field a = UnsafeField (unsafeCoerce a)

mkField :: forall x r a. HasField x r a => a -> Field x r
mkField = UnsafeField . unsafeCoerce

fromField :: HasField x r a => Field x r -> a
fromField (UnsafeField u) = unsafeCoerce u

withField :: HasField x r a => Field x r -> (a -> b) -> b
withField (UnsafeField u) f = f $ unsafeCoerce u

foldField :: HasField x r a => (a -> b) -> Field x r -> b
foldField f (UnsafeField u) = f $ unsafeCoerce u



-- | 'peek's a field of a structure.
peekField :: forall x r a. (HasField x r a, Storable (Field x r)) => Ptr r -> IO a
peekField ptr = fmap (fromField @x @r) . peek $ castPtr ptr

-- | 'poke's a field of a structure.
pokeField :: forall x r a. (HasField x r a, Storable (Field x r)) => Ptr r -> a -> IO ()
pokeField ptr a = poke @(Field x r) (castPtr ptr) $ mkField a

-- | @'poke' . 'getField'@
pokeRecordField :: forall x r a. (HasField x r a, Storable (Field x r)) => Ptr r -> r -> IO ()
pokeRecordField ptr = poke @(Field x r) (castPtr ptr) . mkField . getField @x @r
