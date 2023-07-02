{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module State where

-- The State monad represents a computation that performs some *simulated* mutation
-- Useful for simulating conventional "mutable" state in a pure manner: https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html

reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount count list = (count + 1, reverse list)

appendReversedWithCount :: Int -> [a] -> [a] -> (Int, [a])
appendReversedWithCount count list1 list2 =
  let (funcCount', reversedList1) = reverseWithCount count list1
      (funcCount'', reversedList2) = reverseWithCount funcCount' list2
   in (funcCount'' + 1, reversedList1 ++ reversedList2)

append3ReversedWithCount :: Int -> [a] -> [a] -> [a] -> (Int, [a])
append3ReversedWithCount count list1 list2 list3 =
  let (funcCount', reversedList1) = reverseWithCount count list1
      (funcCount'', reversedList2) = reverseWithCount funcCount' list2
      (funcCount''', reversedList3) = reverseWithCount funcCount'' list3
   in (funcCount''' + 1, reversedList1 ++ reversedList2 ++ reversedList3)

newtype State s a = State {runWithState :: s -> (s, a)}

instance Functor (State s) where
  {-
    Semantics

    Given a stateful computation we apply `f` to the value output of the computation, then return a new stateful computation

    Allows you to transform the results of stateful computations without affecting the state.
  -}
  fmap f (State stateF) =
    State
      ( \s ->
          let (currState, currOutput) = stateF s in (currState, f currOutput)
      )

instance Applicative (State s) where
  {-
    Semantics

    Return a new stateful computation with its output compartment replaced with some value of type `a`
  -}
  pure a = State (\s -> (s, a))

  {-
    Semantics

    We have two stateful computations. The first has it's output component as a function, while the latter has a value in it's output component. We execute the first stateful computation passing in the required state to get the function from it's output component.

    We use the state from the previous computation to perform the next stateful computation to also retrieve the value in the output component of the second stateful

    We then return a new stateful computation with the output compartment being the application of the function gotten from the first stateful computation onto the value from the second stateful computation

    In the context of State, `State stateFX` is a computation that produces a function, and `State stateX` is a computation that produces a value. The <*> operator creates a new computation that applies the function to the value, threading the state through both computations.
  -}
  (State stateFX) <*> (State stateX) =
    State
      ( \s ->
          let (state1, outputFX) = stateFX s
              (state2, outputX) = stateX state1
           in (state2, outputFX outputX)
      )

instance Monad (State s) where
  {-
    Semantics

    Using the output of a previous stateful computation, we create a new stateful computation with the state itself being unchanged
  -}
  (State statefulF) >>= aToS =
    State
      ( \s ->
          let (state, output) = statefulF s
           in (runWithState $ aToS output) state
      )

{-
  Semantics

  Allows us access the state by propagating it to the value compartment of a stateful computation
-}
get :: State s s
get = State (\s -> (s, s))

{-
  Semantics

  Like `pure` or `return`, except it replaces the state instead of the output of the stateful computation. It discards the output
-}
put :: s -> State s ()
put s = State (const (s, ()))

{-
  Semantics

  The composition of `get` and `put`. Transforms the state of a stateful computation by applying some function `f` onto it. Discard the output compartment in the process
-}
modify :: (s -> s) -> State s ()
modify f = get >>= put . f

reverseWithCount' :: [a] -> State Int [a]
reverseWithCount' list = modify (+ 1) >> return (reverse list)

appendReversedWithCount' :: [a] -> [a] -> State Int [a]
appendReversedWithCount' list1 list2 = do
  reversedList1 <- reverseWithCount' list1
  reversedList2 <- reverseWithCount' list2

  return (reverse list1 ++ reverse list2)

append3ReversedWithCount' :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithCount' list1 list2 list3 = do
  reversedList1 <- reverseWithCount' list1
  reversedList2 <- reverseWithCount' list2
  reversedList3 <- reverseWithCount' list3
  return (reversedList1 ++ reversedList2 ++ reversedList3)

-- Stack Impl

type Stack = [Integer]

pop :: State Stack Integer
pop = do
  let popFn (x : xs) = (x, xs)
  state <- get
  let (removedItem, rest) = popFn state
  put rest
  return removedItem

push :: Integer -> State Stack Integer
push x = do
  modify (x :)
  return x

f :: State Stack Integer
f = do
  push 3
  push 4
  push 5

h = runWithState f

chainCPS :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> ((b -> r) -> r)
chainCPS f g b = f $ \a -> g a b

callCC ::
  ((a -> (b -> r) -> r) -> (a -> r) -> r) ->
  (a -> r) ->
  r
callCC f h = f (\a _ -> h a) h
