-- | This module provides a buffer of bounded length which stores elements in
-- the order they're added. When adding an element to a buffer which has
-- reached its maximum length, the oldest element is dropped to make room.
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

data CircularBuffer a = CircularBuffer
    { cbMaxLength :: Int
    , cbData      :: MVar CBData
    , cbContents  :: VM.IOVector a
    }

data CBData = CBData
    { cbStartIdx :: Int
    , cbLength   :: Int
    }

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an empty buffer bounded by 'maxLength'. 'maxLength' must be greater
-- than zero.
empty :: Int -> IO (CircularBuffer a)
empty maxLength = do
    when (maxLength <= 0) $
      error "CircularBuffer.empty: buffer length must be greater than zero"
    contents <- VM.new maxLength
    cbd <- newMVar $ CBData
      { cbStartIdx = 0
      , cbLength   = 0
      }
    return $ CircularBuffer
      { cbMaxLength = maxLength
      , cbContents  = contents
      , cbData      = cbd
      }

-- | Create a buffer from an immutable boxed vector, bounded by 'maxLength'.
-- The seed vector must not be larger than 'maxLength'.
--
-- TODO (AS): We could just truncate the seed vector if it's too big...
fromVector :: Int -> V.Vector a -> IO (CircularBuffer a)
fromVector maxLength initialElems = do
    when (maxLength <= 0) $
      error "CircularBuffer.fromVector: buffer length must be greater than zero"
    when (V.length initialElems > maxLength) $
      error "CircularBuffer.fromVector: seed vector is larger than maxLength"
    contents <- do
        vec <- V.thaw initialElems
        VM.grow vec (maxLength - V.length initialElems)
    cbd <- newMVar $ CBData
      { cbStartIdx = 0
      , cbLength   = V.length initialElems
      }
    return $ CircularBuffer
      { cbMaxLength = maxLength
      , cbContents  = contents
      , cbData      = cbd
      }

fromList :: Int -> [a] -> IO (CircularBuffer a)
fromList maxLength = fromVector maxLength . V.fromList

-------------------------------------------------------------------------------
-- Querying
-------------------------------------------------------------------------------

-- | True if the buffer is empty, False otherwise.
null :: CircularBuffer a -> IO Bool
null cb = (== 0) <$> length cb

-- | 'True' if the buffer is full, 'False' otherwise.
full :: CircularBuffer a -> IO Bool
full cb = (== maxLength cb) <$> length cb

-- | The current number of elements in the given buffer.
length :: CircularBuffer a -> IO Int
length cb = fmap cbLength $ readMVar $ cbData cb

-- | The upper-bound for the number of elements in the given buffer.
maxLength :: CircularBuffer a -> Int
maxLength = cbMaxLength

-------------------------------------------------------------------------------
-- Modifying
-------------------------------------------------------------------------------

-- | Add an element to the top of the buffer. If the buffer is already at its
-- maximum size, this will cause the oldest element to be evicted.
push :: a -> CircularBuffer a -> IO ()
push x cb =
    modifyMVar_ (cbData cb) $ \cbd -> do
      let nextWriteIdx = (cbStartIdx cbd + cbLength cbd) `mod` cbMaxLength cb
      VM.write (cbContents cb) nextWriteIdx x
      if cbLength cbd < cbMaxLength cb
        then return $ cbd { cbLength   =  cbLength   cbd + 1 }
        else return $ cbd { cbStartIdx = (cbStartIdx cbd + 1) `mod` cbMaxLength cb }

-- | Evict the oldest element in the buffer, if it contains any.
evict :: CircularBuffer a -> IO ()
evict cb =
    modifyMVar_ (cbData cb) $ \cbd ->
      return CBData
        { cbStartIdx = (cbStartIdx cbd + 1) `mod` cbMaxLength cb
        , cbLength   = max 0 $ cbLength cbd - 1
        }

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

-- | Create an immutable boxed vector from the elements in the given buffer.
-- The elements are in order from oldest to newest.
toVector :: CircularBuffer a -> IO (V.Vector a)
toVector cb = do
    CBData { cbStartIdx = startIdx, cbLength = len } <- readMVar $ cbData cb
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
