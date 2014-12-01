
# `circular-buffer`

This package provides a buffer of bounded length which stores elements in the
order they're added. When adding an element to a buffer which has reached its
maximum length, the oldest element is dropped to make room.

```haskell
empty     :: Int -> IO (CircularBuffer a)
maxLength :: CircularBuffer a -> Int)
length    :: CircularBuffer a -> IO Int
push      :: a -> CircularBuffer a -> IO ()
evict     :: CircularBuffer a -> IO ()
listElems :: CircularBuffer a -> IO (Vector a)
```

