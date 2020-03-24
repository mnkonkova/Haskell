{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# LANGUAGE Arrows #-}

module HW01 where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Semigroup
import Data.Foldable

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow


newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local r1 (Reader r2) = Reader (r2.r1)

instance Functor (Reader r) where
    -- fmap f (Reader r) = Reader (f.r)
    fmap f reader = Reader (\r -> f (runReader reader r))
instance Applicative (Reader r) where
    pure a = Reader (const a)
    Reader f <*> Reader g = Reader (\r -> f r (g r))
instance Monad (Reader r) where
    -- (>>=) :: m a -> (a -> m b) -> m b
    return x = Reader (const x)
    (>>=) x func  = Reader (\r -> runReader (func (runReader x r)) r)

newtype Writer w a = Writer {runWriter :: (a, w)}

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

listen :: Monoid w => Writer w a -> Writer w (w, a)
listen (Writer (a, w)) = Writer ((w, a), w)

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass (Writer ((a,f),w)) = Writer (a,f w)

instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    -- (<*>) :: f (a -> b) -> f a -> f b
    Writer (f, w1) <*> Writer (a2, w2) = Writer (f a2, w1 `mappend` w2)

instance Monoid w => Monad (Writer w) where
    return a = Writer (a, mempty)
    (>>=) (Writer (a, w)) func  = (\w1 (Writer (a2, w2))  -> Writer(a2, w1 `mappend` w2)) w (func a)


newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\a -> ((), s))

instance Functor (State s) where
    fmap f state = State ((\(a, s) -> (f a, s)).(\a -> runState state a))
                               
instance Applicative (State s) where
    pure a = State (\s -> (a, s))
    func <*> state = State ((\((f, a, s)) -> (f a, s)).(\a -> let (f, s1) = runState func a
                                                                  (a2, s2) = runState state s1
                                                                  in (f, a2, s2)))

instance Monad (State s) where
    return a = State (\s -> (a, s))
    (>>=) state func  =  State ((\(a, s) -> runState (func a) s).(\a -> runState state a))




newtype SignalFunction a b = SF ((a, Double) -> (SignalFunction a b, b, Double))

instance Category SignalFunction where
    id = SF (\(a, time) -> (id, a, time))
    SF f . SF g = SF $ \(a, time) -> let (sfg, b, timeg) = g (a, time)
                                         (sff, c, timef) = f (b, timeg)
                                     in (sff . sfg, c, timef)
instance Arrow SignalFunction where
  arr f = SF (\(a, time) -> (arr f, f a, time))
  first (SF f) = SF (\((a, b), time) -> 
                                  let (sf, x, time_x) = f (a, time)
                                  in (first sf, (x, b), time_x))

accumulated_ :: (Double, Double) -> SignalFunction Double Double
accumulated_ (accumulated, current_x) = SF (\(x2, t2) -> (accumulated_ (t2 * (x2 - current_x) / 2 + current_x * t2 + accumulated, x2),
                                            t2 * (x2 - current_x) / 2 + current_x * t2 + accumulated, x2))

integral :: SignalFunction Double Double
integral = accumulated_ (0, 0)

someFunction :: SignalFunction (Double, Double) (Double, Double)
someFunction = proc (x, y) -> do
    mul1 <- arr (\x -> x * 2)-< x
    mul2 <- arr (\x -> x * 3)-< y
    sum1 <- arr (\(x, y) -> (x + y, x + y)) -< (mul1, mul2)
    sum2 <- (first integral) -< sum1
    returnA -< sum2

runSignalFunction :: SignalFunction a b -> a -> [(a, Double)] -> [b]
runSignalFunction _ _ [] = []
runSignalFunction sf atZero inputs = outputs
  where
  (x:xs) = inputs
  (a, time) = x
  (SF f) = sf
  outputs = 
    let (sigf, b, sum_sq) = f x
    in b:(runSignalFunction sigf a xs)

newtype MyCont r a = MyCont { runCont :: (a -> r) -> r }
instance Functor (MyCont r) where
  fmap f (MyCont cont) = MyCont (\a -> cont (a.f))
--a == \x -> x a1 
-- (a1 type = a)

instance Applicative (MyCont r) where
  pure a = MyCont (\c -> (c a))
  (MyCont f) <*> (MyCont cont) = MyCont (\a -> f (\b ->  cont (a.b)))
  -- a :: b->r 
  -- b :: (a->b)
  -- (\b ->  cont (a . b)) :: (a->b)-> ((a->r)->r) (a->r)  :: (a->b)->r
instance Monad (MyCont r) where
  return a = MyCont (\c -> (c a))
  (>>=) (MyCont cont) func  =  MyCont (\a -> cont (\b -> (runCont (func b)) a))
    -- MyCont (\r -> runCont func (\b -> cont b) r)
    --a :: (b->r)
check_MyCont :: (Integer -> Integer) -> Integer
check_MyCont f = f 2

check_MyCont1 :: ((Integer -> Integer) -> Integer) -> Integer
check_MyCont1 f = f (+3)

check_MyCont2 :: Integer -> MyCont Integer Integer
check_MyCont2 x = MyCont (\x -> x 10)



class MonadTrans n where
  lift :: Monad m => m a -> n m a
-- (>>=) :: m a -> (a -> m b) -> m b
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
   lift ma = MaybeT (ma >>= return . Just)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance MonadTrans (StateT s) where
    lift ma = StateT (\s -> ma >>= (\a -> return (a, s)))

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
    lift ma = ContT (\s -> ma >>= s)
--------------------------------------------
main :: IO ()
main = hspec $ do
    describe "Reader" $ do
        it "ask" $ do
         runReader (Reader (+7)) 5 `shouldBe` 12
         runReader ask [5, 1] `shouldBe` [5, 1]
        it "local" $ do
         runReader (local (+5) ask) 5  `shouldBe` 10
        it "Functor" $ do
         runReader (fmap (+3) (Reader (*7))) 5  `shouldBe` 38
         -- fmap (*3) [5, 1] `shouldBe` [38]
        it "Applicative" $ do
         runReader ((Reader (+)) <*> (Reader (*7))) 5 `shouldBe` (5 * 7 + 5)
        it "Monad" $ do
         runReader ((Reader (+7)) >>= (\r -> (Reader (*r)))) 5 `shouldBe` (5 + 7) * 5
    describe "Writer" $ do
        it "tell" $ do
         runWriter (Writer ([1], [7])) `shouldBe` ([1], [7])
         runWriter (tell [7]) `shouldBe` ((), [7])
        it "listen" $ do
         runWriter (listen (Writer ([1], [7]))) `shouldBe` (([7], [1]), [7])
        it "Functor" $ do
         runWriter (fmap (fmap (+3)) (Writer ([1], [7]))) `shouldBe` ([4], [7])
        it "Applicative" $ do
         runWriter ((Writer ((fmap (+3)), mempty)) <*> (Writer ([1], [7]))) `shouldBe` ([4], [7])
        it "Monad" $ do
         runWriter ((Writer ([1], [7])) >>= (\r-> Writer((r++[3]), mempty))) `shouldBe` ([1, 3], [7])
    describe "State" $ do
        it "get" $ do
         runState (put (1)) 1 `shouldBe` ((), 1)
         runState (get) 1`shouldBe` (1, 1)
        it "Functor" $ do
         runState (fmap (+4) (return 5)) 1 `shouldBe` (9, 1)
        it "Applicative" $ do
         runState (return (*5) <*> return (5)) 1 `shouldBe` (25, 1)
        it "Monad" $ do
         runState (return 'X') 1 `shouldBe` ('X',1)
         runState (do {put 5; return 'X' }) 1 `shouldBe` ('X',5)
    describe "Arrow" $ do
        it "someFunction" $ do
          -- (runSF (someFunction)) ((1.0, 2.0), 0.1) `shouldBe` ((0.4, 8.0), 8.0)
          (let (SF f) = integral
               (integral1 , x1, t1) = f (0.0, 0.0)
               (SF f1) = integral1
               (integral2, acc2, t2) = f1 (8, 0.1)
               (SF f2) = integral2
               (integral3, acc3, t3) = f2 (18, 0.2)
               (SF f3) = integral3
               (integral4, acc, t) = f3 (28, 0.3)
               -- (integral2 , x2, t2) = f1 (0.1, 8)
            in (acc, t)) `shouldBe` (9.899999999999999, 28.0)
          runSignalFunction someFunction (0, 0) [ ((1, 2), 0.1), ((3, 4), 0.2), ((5, 6), 0.3) ]
            `shouldBe` [ (0.4, 8), (3, 18), (9.899999999999999, 28) ]
    describe "MyCont" $ do
        it "Functor" $ do
          (runCont (MyCont check_MyCont)) (\int -> int + 10)`shouldBe` 12
          (runCont (fmap (+3) (MyCont check_MyCont))) (\int -> int + 10)`shouldBe` 15
        it "Applicative" $ do
          (runCont ((MyCont check_MyCont1)<*> (MyCont check_MyCont))) (\int -> int + 10)`shouldBe` 15
        it "Monad" $ do
          runCont ((MyCont check_MyCont) >>= check_MyCont2) (\int -> int + 10) `shouldBe` 20
