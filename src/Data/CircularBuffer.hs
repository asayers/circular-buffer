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
    , read

      -- * Modifying
    , write
    , evict
    , enqueue
    , dequeue

      -- * Deconstruction
    , toVector
    , toList
    ) where

import           Control.Concurrent.MVar
import           Control.Monad       (when)
import           Control.Applicative ((<$>))

import           Data.Maybe          (fromJust, isJust)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import           Prelude             hiding (length, null, read)


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

-- | Returns the oldest element in the buffer, if it contains any elements.
read :: CircularBuffer a -> IO (Maybe a)
read cb = do
    cbd <- readMVar $ cbData cb
    if (cbdLength cbd == 0)
      then return Nothing
      else Just <$> VM.read (cbContents cb) (cbdStartIdx cbd)

-------------------------------------------------------------------------------
-- Modifying
-------------------------------------------------------------------------------

-- | Add an element to the buffer. If the buffer is already at its maximum
-- size, this will cause the oldest element to be evicted.
write :: a -> CircularBuffer a -> IO ()
write x cb =
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

-- | Add an element to the buffer, but only if it isn't already full. Returns
-- 'True' if the write succeeded, and 'False' otherwise.
enqueue :: a -> CircularBuffer a -> IO Bool
enqueue x cb = do
    isFull <- full cb
    if isFull
      then return False
      else write x cb >> return True

-- | Read the oldest element and drop it from the buffer, if the buffer
-- contains any elements.
dequeue :: CircularBuffer a -> IO (Maybe a)
dequeue cb = do
    x <- read cb
    when (isJust x) $ evict cb
    return x


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
