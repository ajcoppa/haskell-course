{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  g <$> (StateT stateTF) = StateT (\s ->
    -- stateTF :: s -> f (a, s)
    -- stateResult :: f (a, s)
    let stateResult = stateTF s
        -- Have an f (a, s) and an (a -> b), need to get f (b, s)
        -- If we have a function with this type:
        applyToFirst :: (a -> b) -> (a, c) -> (b, c)
        applyToFirst f (x,y) = (f x, y)
    -- then we can fmap it over f to get f (b, s)!
    in (applyToFirst g) <$> stateResult)

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) ::
   StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (StateT stF) <*> (StateT stX) = StateT $ \s ->
    let mFWithState = stF s
        applyToFirst :: (a -> b) -> (a, c) -> (b, c)
        applyToFirst f (x,y) = (f x, y)
        bindF (f, s') = applyToFirst f <$> stX s'
    in bindF =<< mFWithState

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  f =<< (StateT stX) = StateT $ \s ->
    stX s >>= \(x, s') ->
    return (f x) >>= \(StateT y) ->
    y s'

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT $ \s -> return (f s)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT sf) s = runId $ sf s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT st) s = snd <$> st s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' st s = snd $ runState' st s

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT st) s = fst <$> st s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' st s = fst $ runState' st s

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT = StateT $ \s -> return (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT x = StateT $ \_ -> return ((), x)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' xs =
  eval (filtering f xs) S.empty where
  f x = State $ \s -> (S.notMember x s, S.insert x s)

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs =
  evalT (filtering f xs) S.empty where
  f x = StateT $ \s ->
    if x > 100
    then Empty
    else Full (S.notMember x s, S.insert x s)

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  f <$> (OptionalT x) = OptionalT $ (mapOptional f) <$> x
  -- or...            = OptionalT $ (f <$>) <$> x

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Applicative f => Applicative (OptionalT f) where
  pure x = OptionalT $ pure (Full x)

  (OptionalT foF) <*> (OptionalT foX) = OptionalT $
    (twiceOptional ($)) <$> foF <*> foX
  {- Explanation: twiceOptional feeds two Optionals into a function,
  returning Empty if either argument is Empty. If we make our function
  ($), it'll apply a function we give it to a value we give it, returning
  Empty if either is Empty!

  But we don't directly have Optionals, we have Optionals wrapped inside
  another instance of Apply, so we have to <$> and <*> our twiceOptional'ed
  ($) to get inside the Apply instance to the inner Optional values.

  Note: lift2 could also be used as a more general twiceOptional. -}

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  g =<< otMOX =
    {-
    =<< :: (a -> OptionalT m b)
        -> OptionalT m a
        -> OptionalT m b

    g :: (a -> OptionalT m b)
    otMOX :: OptionalT m a

    otMOX stands for an OptionalT wrapper around an m (Optional a).

    Since g takes an a and gives us the OptionalT m b that we ultimately want,
    let's work to deconstruct our OptionalT m a into an a.

    First, let's strip the OptionalT covering with runOptionalT.
    -}
    let mOX = runOptionalT otMOX
    {-
    That leaves us with an m (Optional a). If we use the fact that m is a monad
    to bind over it, then inside the binding we can treat it as an Optional a.

    What should the type signature for our helper bound function look like?
    * It needs to return an m (Optional b) since we're passing in an
    m (Optional a).
    * It should take an Optional a value since that's what we'll get when
    we feed =<< an m (Optional a).
    * We'll want to run g inside the binding since that's the whole purpose
    of unwrapping our starting value, so it needs to take in g, which has type
    (a -> OptionalT m b).
    -}
        helper :: Monad m =>
                  (a -> OptionalT m b)
                  -> Optional a
                  -> m (Optional b)
    {-
    Hmm. What should it do to fulfill that signature?

    Well, if our value is Empty we don't need to actually run g. We just need
    to wrap the Empty up into a default monadic context, so pure should work,
    and it'll give us the m (Optional b) result we want.
    -}
        helper _ Empty = pure Empty
    {-
    If our value is Full x, we can extract the x and run g with it. But we get
    back an entire OptionalT m b, so we need to strip off the OptionalT wrapper
    to satisfy our return type.
    -}
        helper g (Full x) = runOptionalT $ g x
    {-
    Now we've got an m (Optional b), and we've run g, fulfilling our goal.

    All we have to do now is add an outer OptionalT wrapper, and we're done!
    -}
    in OptionalT $ helper g =<< mOX

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> (Logger ls x) = Logger ls (f x)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger Nil

  (Logger fL f) <*> (Logger xL x) =
    Logger (fL ++ xL) (f x)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  f =<< (Logger xL x) =
    -- f :: a -> Logger l b
    let newLogger = f x
        getLog :: (Logger l a) -> List l
        getLog (Logger ls _) = ls
        getValue :: (Logger l a) -> a
        getValue (Logger _ z) = z
        newLogs = getLog newLogger
        y = getValue newLogger
    in Logger (xL ++ newLogs) (y)

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l x = Logger (l :. Nil) x

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG xs =
  -- filtering ::
  --   Applicative f =>
  --   (a -> f Bool)
  --   -> List a
  --   -> f (List a)
  runOptionalT $ evalT (filtering f xs) S.empty where
    f :: (Integral a, Show a) =>
         a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
    f x = StateT $ \s ->
      -- base case to get types right:
      -- OptionalT $ Logger Nil Empty
      let xStr = listh $ show x
          message = if x > 100
            then ("aborting > 100: " ++ xStr) :. Nil
            else if even x
              then ("even number: " ++ xStr) :. Nil
              else Nil
          optValue = if x > 100
            then Empty
            else Full (S.notMember x s, S.insert x s)
      in OptionalT $ Logger message optValue
