module Writer where

-- The Writer monad represents a computation that produces a value and accumulates a monoidic secondary output. Monoidic because the outputs must be concatenated through each computation
-- The Writer monad represents computations with “extra output”: produced data, logging messages, and so forth: https://journal.infinitenegativeutility.com/writer-monads-and-space-leaks
-- Useful for simulating conventional "mutable" state in a pure manner: https://williamyaoh.com/posts/2020-07-26-deriving-writer-monad.html

newtype Writer log a = Writer {runWriter :: (log, a)}

instance Functor (Writer log) where
  {-
    Semantics

    Given a computation that outputs a value and a log, we transform the value with a function `f`, leaving the log untouched
  -}
  fmap f (Writer (log, output)) = Writer (log, f output)

instance (Monoid log) => Applicative (Writer log) where
  {-
    Semantics

    Lift a value x into a Writer result, initializing the log with it's identity (since it must be a monoid)
  -}
  pure x = Writer (mempty, x)

  {-
    Semantics

    Given a Writer with a value that is a function and a Writer with a value that is..well, a value, we return a new Writer, with the logs from both writers concatenated, and the function from the first writer applied to the value of the second
  -}
  (Writer (log1, f)) <*> (Writer (log2, x)) = Writer (log1 `mappend` log2, f x)

instance (Monoid log) => Monad (Writer log) where
  {-
    Semantics

    Threads the value from the first writer to another writer (gotten from applying said value to the function that return a writer). Then, a new writer is generated with logs from both writers concatenated and the value from the last writer is used as the value for the new writer
  -}
  (Writer (log1, v1)) >>= aToW =
    let (log2, v2) = runWriter $ aToW v1 in Writer (log1 `mappend` log2, v2)

{-
  Semantics

  Creates a new Writer from a log with no computation output. A way to produce a log without producing a result.
-}
tell :: log -> Writer log ()
tell log = Writer (log, ())

{-
  Semantics

  Transform a log with a function `f`. Leaving the output unchanged. A way to transform the log of a computation without affecting the result.
-}
censor :: (log -> log) -> Writer log a -> Writer log a
censor f (Writer (log, v)) = Writer (f log, v)

{-
  Semantics

  A way to include the logs with a result to make the log accessible for computation
-}
listen :: Writer log a -> Writer log (a, log)
listen (Writer (log, x)) = Writer (log, (x, log))

-- Before the Writer Monad

addTwo :: Int -> ([String], Int)
addTwo x = (["adding 2..."], x + 2)

augmentAndStringify :: Int -> Int -> ([String], String)
augmentAndStringify x y =
  let (xLog, x') = addTwo x
      (yLog, y') = addTwo y
   in (["augmenting..."] ++ xLog ++ yLog ++ ["stringifying..."], show (x' + y'))


