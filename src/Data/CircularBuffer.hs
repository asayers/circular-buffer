-- | This module provides a mutable buffer of bounded length which stores
-- elements in the order they're added. When adding an element to a buffer
-- which has reached its maximum length, the oldest element is dropped to make
-- room.
--
-- This module is intended to be imported qualified.
module Data.CircularBuffer
    ( CircularBuffer

      -- * Construction
    , empty
    , fromVector
    , fromList

      -- * Querying
    , null
    , full
    , length
    , maxLength

      -- * Modifying
    , push
    , evict

      -- * Deconstruction
    , toVector
    , toList
    ) where

import           Control.Concurrent.MVar
import           Control.Monad       (when)
import           Control.Applicative ((<$>))

import           Data.Maybe          (fromJust)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import           Prelude             hiding (length, null)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A mutable buffer which contains a bounded number of boxed elements of type
-- @a@.
data CircularBuffer a = CircularBuffer
    { cbMaxLength :: Int
    , cbData      :: MVar CBData
    , cbContents  :: VM.IOVector a
    }

data CBData = CBData
    { cbdStartIdx :: Int
    , cbdLength   :: Int
    }

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an empty buffer bounded by @maxLength@. This bound must be greater
-- than zero.
empty :: Int -> IO (CircularBuffer a)
empty maxLength = do
    when (maxLength <= 0) $
      error "CircularBuffer.empty: upper bound must be greater than zero"
    contents <- VM.new maxLength
    cbd <- newMVar $ CBData
      { cbdStartIdx = 0
      , cbdLength   = 0
      }
    return $ CircularBuffer
      { cbMaxLength = maxLength
      , cbContents  = contents
      , cbData      = cbd
      }

-- TODO (AS): We could just truncate the seed vector if it's too big...
--
-- | Create a buffer from an immutable boxed vector, bounded by @maxLength@.
-- The seed vector must not be larger than the upper bound.
fromVector :: Int -> V.Vector a -> IO (CircularBuffer a)
fromVector maxLength initialElems = do
    when (maxLength <= 0) $
      error "CircularBuffer.fromVector: upper bound must be greater than zero"
    when (V.length initialElems > maxLength) $
      error "CircularBuffer.fromVector: seed vector is too big"
    contents <- do
        vec <- V.thaw initialElems
        VM.grow vec (maxLength - V.length initialElems)
    cbd <- newMVar $ CBData
      { cbdStartIdx = 0
      , cbdLength   = V.length initialElems
      }
    return $ CircularBuffer
      { cbMaxLength = maxLength
      , cbContents  = contents
      , cbData      = cbd
      }

-- | Create a buffer from a list, bounded by @maxLength@. The seed list must
-- not be larger than the upper bound.
fromList :: Int -> [a] -> IO (CircularBuffer a)
fromList maxLength = fromVector maxLength . V.fromList

-------------------------------------------------------------------------------
-- Querying
-------------------------------------------------------------------------------

-- | 'True' if the buffer is empty, 'False' otherwise.
null :: CircularBuffer a -> IO Bool
null cb = (== 0) <$> length cb

-- | 'True' if the buffer is full, 'False' otherwise.
full :: CircularBuffer a -> IO Bool
full cb = (== maxLength cb) <$> length cb

-- | The current number of elements in the given buffer.
length :: CircularBuffer a -> IO Int
length cb = fmap cbdLength $ readMVar $ cbData cb

-- | The upper-bound for the number of elements in the given buffer.
maxLength :: CircularBuffer a -> Int
maxLength = cbMaxLength

-------------------------------------------------------------------------------
-- Modifying
-------------------------------------------------------------------------------

-- | Add an element to the buffer. If the buffer is already at its maximum
-- size, this will cause the oldest element to be evicted.
push :: a -> CircularBuffer a -> IO ()
push x cb =
    modifyMVar_ (cbData cb) $ \cbd -> do
      let nextWriteIdx = (cbdStartIdx cbd + cbdLength cbd) `mod` cbMaxLength cb
      VM.write (cbContents cb) nextWriteIdx x
      if cbdLength cbd < cbMaxLength cb
        then return $ cbd { cbdLength   =  cbdLength   cbd + 1 }
        else return $ cbd { cbdStartIdx = (cbdStartIdx cbd + 1) `mod` cbMaxLength cb }

-- | Drop the oldest element from the buffer, if it contains any elements.
evict :: CircularBuffer a -> IO ()
evict cb =
    modifyMVar_ (cbData cb) $ \cbd ->
      return CBData
        { cbdStartIdx = (cbdStartIdx cbd + 1) `mod` cbMaxLength cb
        , cbdLength   = max 0 $ cbdLength cbd - 1
        }

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

-- | Create an immutable boxed vector from the elements in the given buffer.
-- The elements are in order from oldest to newest.
toVector :: CircularBuffer a -> IO (V.Vector a)
toVector cb = do
    CBData { cbdStartIdx = startIdx, cbdLength = len } <- readMVar $ cbData cb
    let highestIdx = cbMaxLength cb
        takeSlice sliceStart sliceLength =
          V.freeze $ VM.slice sliceStart sliceLength (cbContents cb)

    if startIdx + len <= highestIdx
      then takeSlice startIdx len
      else V.concat <$> sequence
        [ takeSlice startIdx (highestIdx - startIdx)
        , takeSlice 0 (startIdx + len - highestIdx)
        ]

-- | Create a list from the elements in the given buffer. The elements are in
-- order from oldest to newest.
toList :: CircularBuffer a -> IO [a]
toList = fmap V.toList . toVector
