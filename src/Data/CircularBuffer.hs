-- | A buffer of bounded length which stores elements in the order they're
-- added. When adding an element to a buffer which has reached its maximum
-- length, the oldest element is dropped to make room.
module Data.CircularBuffer
    ( CircularBuffer
    , empty
    , length
    , maxLength
    , push
    , evict
    , listElems
    ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Applicative ((<$>))

import           Data.Maybe          (fromJust)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import           Prelude             hiding (length)

data CircularBuffer a = CircularBuffer
    { cbMaxLength :: Int
    , cbData      :: MVar CBData
    , cbContents  :: VM.IOVector a
    }

data CBData = CBData
    { cbStartIdx :: Int
    , cbLength   :: Int
    }

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

maxLength :: CircularBuffer a -> Int
maxLength = cbMaxLength

length :: CircularBuffer a -> IO Int
length cb = fmap cbLength $ readMVar $ cbData cb

-- | Add an element into the buffer. If the buffer is already at its maximum
-- size, this will cause the oldest element to be evicted.
push :: a -> CircularBuffer a -> IO ()
push x cb =
    modifyMVar_ (cbData cb) $ \cbd -> do
      let nextWriteIdx = (cbStartIdx cbd + cbLength cbd) `mod` cbMaxLength cb
      VM.write (cbContents cb) nextWriteIdx x
      if cbLength cbd < cbMaxLength cb
        then return $ cbd { cbLength   =  cbLength   cbd + 1 }
        else return $ cbd { cbStartIdx = (cbStartIdx cbd + 1) `mod` cbMaxLength cb }

-- | Evict the oldest element in the buffer, if there are any elements.
evict :: CircularBuffer a -> IO ()
evict cb =
    modifyMVar_ (cbData cb) $ \cbd ->
      return CBData
        { cbStartIdx = (cbStartIdx cbd + 1) `mod` cbMaxLength cb
        , cbLength   = max 0 $ cbLength cbd - 1
        }

listElems :: CircularBuffer a -> IO (V.Vector a)
listElems cb = do
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


