{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Word.Extra where

#include "MachDeps.h"

import Data.Array.Byte (ByteArray (..))
#ifndef WORDS_BIGENDIAN
import Data.Binary (byteSwap32)
#endif
import Data.Word (Word32, Word64)
#if WORD_SIZE_IN_BITS == 64
#ifdef WORDS_BIGENDIAN
import GHC.Base (Int (..), indexWord8ArrayAsWord64#, indexWord8ArrayAsWord32#)
#else
import GHC.Base (Int (..), byteSwap64#, indexWord8ArrayAsWord64#, indexWord8ArrayAsWord32#)
#endif
#elif WORD_SIZE_IN_BITS == 32
import GHC.Base (Int (..), indexWord8ArrayAsWord32#)
#endif
#if WORD_SIZE_IN_BITS == 64
import GHC.Word (Word64 (..), Word32 (..))
#elif WORD_SIZE_IN_BITS == 32
import GHC.Word (Word32 (..))
#endif
#if WORD_SIZE_IN_BITS == 32
import Data.Bits (Bits(..), (.|.))
#endif

#if WORD_SIZE_IN_BITS == 64

#ifdef WORDS_BIGENDIAN

indexWord64BE :: ByteArray -> Int -> Word64
indexWord64BE (ByteArray ba#) (I# i#) =
  W64# (indexWord8ArrayAsWord64# ba# i#)

#else

indexWord64BE :: ByteArray -> Int -> Word64
indexWord64BE (ByteArray ba#) (I# i#) =
  W64# (byteSwap64# (indexWord8ArrayAsWord64# ba# i#))

#endif

#elif WORD_SIZE_IN_BITS == 32

indexWord64BE :: ByteArray -> Int -> Word64
indexWord64BE ba i =
  (fromIntegral (indexWord32BE ba i) `shiftL` 32) .|. fromIntegral (indexWord32BE ba (i + 4))

#endif
{-# INLINE indexWord64BE #-}

#ifdef WORDS_BIGENDIAN
indexWord32BE :: ByteArray -> Int -> Word32
indexWord32BE (ByteArray ba#) (I# i#) = W32# (indexWord8ArrayAsWord32# ba# i#)
#else
indexWord32BE :: ByteArray -> Int -> Word32
indexWord32BE (ByteArray ba#) (I# i#) = byteSwap32 $ W32# (indexWord8ArrayAsWord32# ba# i#)
#endif
{-# INLINE indexWord32BE #-}
