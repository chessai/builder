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
  , unaligned
  , word8
  , word16
  , word32
  , word64
  , word
  , int8
  , int16
  , int32
  , int64
  , int
  , float
  , double
  , char
  , ptr
  , bytearray
  , fixed
  ) where

import Data.Primitive hiding (writeByteArray)
import Data.Primitive.ByteArray.Unaligned
import Data.Int
import Data.Word
import GHC.Exts hiding (build)
import System.ByteOrder

-- | A 'Builder' for 'ByteArray's that has O(1) append.
--   To create a 'ByteArray', use 'build'. This will only
--   do one allocation.
data Builder = Builder
  { size   :: Int#
  , writer :: forall s. ()
      => MutableByteArray# s
      -> Int#
      -> (State# s -> (# State# s, Int# #))
  }

instance Semigroup Builder where
  Builder len0 w0 <> Builder len1 w1 = Builder
    { size   = len0 +# len1
    , writer = \marr# ix0# s0# ->
        case w0 marr# ix0# s0# of
          (# s1#, ix1# #) -> w1 marr# ix1# s1#
    }

instance Monoid Builder where
  mempty = Builder
    { size   = 0#
    , writer = \_ ix0# s0# -> (# s0#, ix0# #)
    }

type Writer# s = MutableByteArray# s
  -> Int#
  -> (State# s -> (# State# s, Int# #))

runWriter# :: ()
  => Int#
  -> Writer# s
  -> State# s
  -> (# State# s, ByteArray# #)
runWriter# sz# g = \s0# -> case newByteArray# sz# s0# of
  (# s1#, marr# #) -> case g marr# 0# s1# of
    (# s2#, _ #) -> case unsafeFreezeByteArray# marr# s2# of
      (# s3#, b# #) -> (# s3#, b# #)
{-# inline runWriter# #-}

-- | Convert a 'Builder' into a 'ByteArray'.
build :: Builder -> ByteArray
build (Builder len# w) = case runRW# (runWriter# len# w) of
  (# _, b# #) -> ByteArray b#
{-# inline build #-}

writeUnaligned :: (Prim a, PrimUnaligned a)
  => a
  -> Writer# s
writeUnaligned a = \marr# ix0# s0# ->
  case writeUnalignedByteArray# marr# ix0# a s0# of
    s1# -> (# s1#, ix0# +# alignment# a #)
{-# inline writeUnaligned #-}

writeByteArray :: ()
  => ByteArray
  -> Int
  -> Int
  -> Writer# s
writeByteArray (ByteArray src#) (I# off#) (I# len#)
  = \marr# ix0# s0# ->
      case copyByteArray# src# off# marr# ix0# len# s0# of
        s1# -> (# s1#, ix0# +# len# #)
{-# inline writeByteArray #-}

-- | A 'Builder' for any 'Prim' and 'PrimUnaligned' value.
unaligned :: (Prim a, PrimUnaligned a)
  => a
  -> Builder
unaligned a = Builder (sizeOf# a) (writeUnaligned a)
{-# inline unaligned #-}

-- | A 'Builder' for 'Word8'.
word8 :: Word8 -> Builder
word8 = unaligned
{-# inline word8 #-}

-- | A 'Builder' for 'Word16'.
word16 :: Word16 -> Builder
word16 = unaligned
{-# inline word16 #-}

-- | A 'Builder' for 'Word32'.
word32 :: Word32 -> Builder
word32 = unaligned
{-# inline word32 #-}

-- | A 'Builder' for 'Word64'.
word64 :: Word64 -> Builder
word64 = unaligned
{-# inline word64 #-}

-- | A 'Builder' for 'Word'.
word :: Word -> Builder
word = unaligned
{-# inline word #-}

-- | A 'Builder' for 'Int8'.
int8 :: Int8 -> Builder
int8 = unaligned
{-# inline int8 #-}

-- | A 'Builder' for 'Int16'.
int16 :: Int16 -> Builder
int16 = unaligned
{-# inline int16 #-}

-- | A 'Builder' for 'Int32'.
int32 :: Int32 -> Builder
int32 = unaligned
{-# inline int32 #-}

-- | A 'Builder' for 'Int64'.
int64 :: Int64 -> Builder
int64 = unaligned
{-# inline int64 #-}

-- | A 'Builder' for 'Int'.
int :: Int -> Builder
int = unaligned
{-# inline int #-}

-- | A 'Builder' for 'Char'.
char :: Char -> Builder
char = unaligned
{-# inline char #-}

-- | A 'Builder' for a 'ByteArray' slice.
bytearray :: ()
  => ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> Builder
bytearray b o n@(I# n#) = Builder n# (writeByteArray b o n)
{-# inline bytearray #-}

-- | A 'Builder' for 'Float'.
float :: Float -> Builder
float = unaligned
{-# inline float #-}

-- | A 'Builder' for 'Double'.
double :: Double -> Builder
double = unaligned
{-# inline double #-}

-- | A 'Builder' for @'Ptr' a@.
ptr :: Ptr a -> Builder
ptr = unaligned
{-# inline ptr #-}

-- | A 'Builder' for @'Fixed' b a@
--   This provides better control over endianness when writing.
fixed :: (FixedOrdering b, Bytes a, Prim a, PrimUnaligned a)
  => Fixed b a
  -> Builder
fixed = unaligned
{-# inline fixed #-}
