{-# language
    BangPatterns
  , MagicHash
  , RankNTypes
  , UnboxedTuples
  #-}

-- | A 'Builder' type for 'ByteArray'. Appending these builders
--   can be cheaper than when appending 'ByteArray' values, since
--   you only perform one buffer allocation.
module Builder
  ( -- * Builder type
    Builder

    -- * Consumption
  , build

    -- * Construction
  , buildUnaligned
  , buildWord8
  , buildWord16
  , buildWord32
  , buildWord64
  , buildWord
  , buildInt8
  , buildInt16
  , buildInt32
  , buildInt64
  , buildInt
  , buildFloat
  , buildDouble
  , buildChar
  , buildPtr
  , buildByteArray
  , buildFixed
  ) where

import Data.Primitive hiding (writeByteArray)
import Data.Primitive.ByteArray.Unaligned
import Data.Int
import Data.Word
import GHC.Exts hiding (build)
import System.ByteOrder

newtype Writer s = W
  { _w :: ()
      => MutableByteArray# s
      -- buffer
      -> Int#
      -- offset into buffer
      -> (State# s -> (# State# s, Int# #))
      -- update to offset
  }

instance Semigroup (Writer s) where
  W x <> W y = W $ \marr# ix0# s0# -> case x marr# ix0# s0# of
    (# s1#, ix1# #) -> y marr# ix1# s1#
  {-# inline (<>) #-}

instance Monoid (Writer s) where
  mempty = W $ \_ ix0# s0# -> (# s0#, ix0# #)
  {-# inline mempty #-}

data BI s = BI !Int !(Writer s)

instance Semigroup (BI s) where
  BI len0 w0 <> BI len1 w1 = BI (len0 + len1) (w0 <> w1)
  {-# inline (<>) #-}

instance Monoid (BI s) where
  mempty = BI 0 mempty
  {-# inline mempty #-}

-- | A 'Builder' for 'ByteArray's that has O(1) append.
--   To create a 'ByteArray', use 'build'. This will only
--   do one allocation.
newtype Builder = Builder (forall s. BI s)

instance Semigroup Builder where
  Builder b0 <> Builder b1 = Builder (b0 <> b1)
  {-# inline (<>) #-}

instance Monoid Builder where
  mempty = Builder mempty
  {-# inline mempty #-}

runWriter# :: Int# -> Writer s -> State# s -> (# State# s, ByteArray# #)
runWriter# sz# (W g) = \s0# -> case newByteArray# sz# s0# of
  (# s1#, marr# #) -> case g marr# 0# s1# of
    (# s2#, _ #) -> case unsafeFreezeByteArray# marr# s2# of
      (# s3#, b# #) -> (# s3#, b# #)
{-# inline runWriter# #-}

-- | Convert a 'Builder' into a 'ByteArray'.
build :: Builder -> ByteArray
build (Builder (BI (I# len#) w)) = case runRW# (runWriter# len# w) of
  (# _, b# #) -> ByteArray b#
{-# inline build #-}

writeUnaligned :: (Prim a, PrimUnaligned a)
  => a
  -> Writer s
writeUnaligned a = W $ \marr# ix0# s0# ->
  case writeUnalignedByteArray# marr# ix0# a s0# of
    s1# -> (# s1#, ix0# +# alignment# a #)
{-# inline writeUnaligned #-}

writeByteArray :: ()
  => ByteArray
  -> Int
  -> Int
  -> Writer s
writeByteArray (ByteArray src#) (I# off#) (I# len#)
  = W $ \marr# ix0# s0# ->
      case copyByteArray# src# off# marr# ix0# len# s0# of
        s1# -> (# s1#, ix0# +# len# #)
{-# inline writeByteArray #-}

-- | A 'Builder' for any 'Prim' and 'PrimUnaligned' value.
buildUnaligned :: (Prim a, PrimUnaligned a)
  => a
  -> Builder
buildUnaligned a = Builder (BI (sizeOf a) (writeUnaligned a))
{-# inline buildUnaligned #-}

-- | A 'Builder' for 'Word8'.
buildWord8 :: Word8 -> Builder
buildWord8 = buildUnaligned
{-# inline buildWord8 #-}

-- | A 'Builder' for 'Word16'.
buildWord16 :: Word16 -> Builder
buildWord16 = buildUnaligned
{-# inline buildWord16 #-}

-- | A 'Builder' for 'Word32'.
buildWord32 :: Word32 -> Builder
buildWord32 = buildUnaligned
{-# inline buildWord32 #-}

-- | A 'Builder' for 'Word64'.
buildWord64 :: Word64 -> Builder
buildWord64 = buildUnaligned
{-# inline buildWord64 #-}

-- | A 'Builder' for 'Word'.
buildWord :: Word -> Builder
buildWord = buildUnaligned
{-# inline buildWord #-}

-- | A 'Builder' for 'Int8'.
buildInt8 :: Int8 -> Builder
buildInt8 = buildUnaligned
{-# inline buildInt8 #-}

-- | A 'Builder' for 'Int16'.
buildInt16 :: Int16 -> Builder
buildInt16 = buildUnaligned
{-# inline buildInt16 #-}

-- | A 'Builder' for 'Int32'.
buildInt32 :: Int32 -> Builder
buildInt32 = buildUnaligned
{-# inline buildInt32 #-}

-- | A 'Builder' for 'Int64'.
buildInt64 :: Int64 -> Builder
buildInt64 = buildUnaligned
{-# inline buildInt64 #-}

-- | A 'Builder' for 'Int'.
buildInt :: Int -> Builder
buildInt = buildUnaligned
{-# inline buildInt #-}

-- | A 'Builder' for 'Char'.
buildChar :: Char -> Builder
buildChar = buildUnaligned
{-# inline buildChar #-}

-- | A 'Builder' for a 'ByteArray' slice.
buildByteArray :: ()
  => ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> Builder
buildByteArray b o n = Builder (BI n (writeByteArray b o n))
{-# inline buildByteArray #-}

-- | A 'Builder' for 'Float'.
buildFloat :: Float -> Builder
buildFloat = buildUnaligned
{-# inline buildFloat #-}

-- | A 'Builder' for 'Double'.
buildDouble :: Double -> Builder
buildDouble = buildUnaligned
{-# inline buildDouble #-}

-- | A 'Builder' for @'Ptr' a@.
buildPtr :: Ptr a -> Builder
buildPtr = buildUnaligned
{-# inline buildPtr #-}

-- | A 'Builder' for @'Fixed' b a@
--   This provides better control over endianness when writing.
buildFixed :: (FixedOrdering b, Bytes a, Prim a, PrimUnaligned a)
  => Fixed b a
  -> Builder
buildFixed = buildUnaligned
{-# inline buildFixed #-}
