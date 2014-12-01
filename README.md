
# `circular-buffer`

This package provides a buffer of bounded length which stores elements in the
order they're added. When adding an element to a buffer which has reached its
maximum length, the oldest element is dropped to make room.

```haskell
-- * Construction
empty      :: Int -> IO (CircularBuffer a)
fromVector :: Int -> V.Vector a -> IO (CircularBuffer a)
fromList   :: Int -> [a] -> IO (CircularBuffer a)

-- * Querying
maxLength  :: CircularBuffer a -> Int)
length     :: CircularBuffer a -> IO Int

-- * Modifying
push       :: a -> CircularBuffer a -> IO ()
evict      :: CircularBuffer a -> IO ()

-- * Deconstruction
toVector   :: CircularBuffer a -> IO (V.Vector a)
toList     :: CircularBuffer a -> IO [a]
```

