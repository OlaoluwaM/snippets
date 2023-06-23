{-# LANGUAGE MultiParamTypeClasses #-}

module Reader where

-- Useful for read-only state across a chain of computations: https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html

import Data.Char qualified as Char

data ABConfig = ABConfig {noUseLetterE :: Bool, noUseLetterL :: Bool}

toUpperStr :: ABConfig -> String -> String
toUpperStr cfg str =
  let ch = (Char.toUpper <$> str) in filter passesFilter ch
  where
    filters =
      [ if noUseLetterE cfg then (/= 'E') else const True,
        if noUseLetterL cfg then (/= 'L') else const False
      ]
    passesFilter :: Char -> Bool
    passesFilter c = all (\f -> f c) filters

welcomeMessage :: ABConfig -> String -> String -> String
welcomeMessage cfg motd username =
  "Welcome, "
    ++ toUpperStr cfg username
    ++ "! Message of the day: "
    ++ toUpperStr cfg motd

fullName :: ABConfig -> String -> String -> String -> String
fullName cfg fn nn ln = let toUpperWithConfig = toUpperStr cfg in toUpperWithConfig fn ++ " \"" ++ toUpperWithConfig nn ++ "\" " ++ toUpperWithConfig ln

newtype Reader cfg a = Reader {runReader :: cfg -> a}

instance Show (Reader cfg a) where
  show (Reader cfgF) = "Reader"

instance Functor (Reader cfg) where
  {-
    Semantics

    In the context of the Reader type, the `fmap` function is applying a function (`f`) to the result of the computation (`cfgF`), not to the environment. `cfgF` is a computation that takes an environment and produces a result. `fmap f cfgF` applies the function f to the result of that computation.

    It's similar to CPS in that we transform the end result of a Reader's computation: "If you have a function and a Reader computation, you can create a new Reader computation that applies the function to the result of the original computation."

    Once the Reader computation has been supplied its environment (or within the right environment), apply the function `f` to the result of the computation
  -}
  fmap f (Reader cfgF) = Reader (fmap f cfgF)

instance Applicative (Reader cfg) where
  {-
    Semantics

    The pure function takes a value and puts it into the context of the applicative (Reader). In this case, it's creating a Reader computation that ignores its environment and always returns the value a.
    -}
  pure a = Reader (const a)

  {-
    Semantics

    In the context of the Reader type, both `Reader cfgF` and `Reader cfgX` are computations that take an environment and produce a result. The expression (\cfg -> cfgF cfg (cfgX cfg)) is a new computation that, when given an environment, applies the function from `cfgF` to the result of `cfgX`.

    So, in simpler terms, this code is saying: "If you have a Reader computation that produces a function (given some environment) and another Reader computation that produces a result given the same environment, you can create a new Reader computation that applies the function from the first computation to the result of the second Reader computation, all within the same environment."

    Given the same environment apply the function from the first Reader computation to the value from the second Reader computation
  -}
  (Reader cfgF) <*> (Reader cfgX) = Reader (\cfg -> cfgF cfg (cfgX cfg))

instance Monad (Reader cfg) where
  {-
    Semantics

    In the context of the `Reader` type, `Reader cfgF` is a computation that, given an environment, produces a result. `aToR` is a function that takes a normal value and returns a Reader computation.

    The expression `(\cfg -> runReader (aToR (cfgF cfg)) cfg)` is a new computation that, when given an environment, applies it to the `cfgF` computation to get a result, then passes that result to the `aToR` function to get a new Reader computation. Finally, that "new Reader computation" is then executed with the same environment.

    `runReader` is used to destructure the "new Reader Computation" out so we can execute it with it's environment and get a reult

    So, in simpler terms, this code is saying: "If you have a Reader computation and a function that, given a normal value, returns a Reader computation, you can create a new Reader computation that applies the function (`aToR`) to the result from the Reader computation, creating a new Reader computation, then executes the resulting Reader computation with the same environment."
  -}
  (Reader cfgF) >>= aToR = Reader (\cfg -> runReader (aToR (cfgF cfg)) cfg)

{-
  Semantics

  `ask` creates a Reader computation that, when given an environment, simply returns that environment (passes it on). `ask` is a computation that retrieves the current environment.
-}
ask :: Reader cfg cfg
ask = Reader id

g = runReader ask

{-
  Semantics

  An alias for the `Reader` constructor. Given a function, it creates a Reader computation that applies that function to its environment.
-}
asks :: (cfg -> a) -> Reader cfg a
asks = Reader

{-
  Semantics

  A function that modifies the environment required by a Reader computation. It creates a new Reader computation that applies the `envTransformer` function to the environment of the passed-in Reader computation, it then runs the `cfgF` computation with the resulting (transformed) environment

  First transforms the environment for a Reader computation, then creates a new Reader that uses that environment
-}
local :: (oldCfg -> newCfg) -> Reader newCfg a -> Reader oldCfg a
local envTransformer (Reader cfgF) = asks $ cfgF . envTransformer

toUpperStr' :: String -> Reader ABConfig String
toUpperStr' str = do
  cfg <- ask
  let chs = Char.toUpper <$> str

  let filters = [if noUseLetterE cfg then (/= 'E') else const True, if noUseLetterL cfg then (/= 'L') else const False]

  let passesFilter c = all (\f -> f c) filters

  return (filter passesFilter chs)

welcomeMessage' :: String -> String -> Reader ABConfig String
welcomeMessage' motd username = do
  upperMotd <- toUpperStr' motd
  upperUsername <- toUpperStr' username
  return
    ( "Welcome, "
        ++ upperUsername
        ++ "! Message of the day: "
        ++ upperMotd
    )

fullName' :: String -> String -> String -> Reader ABConfig String
fullName' fn nn ln = do
  upperFn <- toUpperStr' fn
  upperNn <- toUpperStr' nn
  upperLn <- toUpperStr' ln
  return (upperFn ++ " \"" ++ upperNn ++ "\" " ++ upperLn)
