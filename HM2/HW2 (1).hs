{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module HW02 where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow

--------------------------------------- Стрелки

{-
1.1 Реализовать категорию стрелок, хранящую количество прошедших сигналов, и инстансы Arrow, ArrowChoice.
-}

data SignalFunction a b
instance Category SignalFunction
instance Arrow SignalFunction

{-
1.2 Реализовать стрелку, задерживающую сигнал на один тик.
-}

-- | Аргумент - выход для первого входного значения.
delay :: b -> SignalFunction a b
delay = _

{-
1.3 Используя расширение -XArrows реализовать схему:

     +=========+
---- | delay 1 | ---
     +=========+    \   +==========+
                     -- | multiply | ------
                    /   +==========+
--------------------

Эта схема умножает второй аргумент на задержанный первый
-}

weirdMultiplier :: SignalFunction (Int, Int) Int
weirdMultiplier = _

{-
1.4 Написать функцию, запускающую схему на списке входных данных

runSignalFunction weirdMultiplier [(1,2), (3, 4), (5, 6)] = [1*2, 1*4, 3*6]
-}

runSignalFunction :: SignalFunction a b -> [a] -> [b]
runSignalFunction = _

--------------------------------------- Решение

{-
newtype SignalFunction a b = SF (a -> (SignalFunction a b, b))

instance Category SignalFunction where
  id = SF (\a -> (id, a))
  SF f . SF g = SF $ \a -> let (sfg, b) = g a
                               (sff, c) = f b
                            in (sff . sfg, c)

instance Arrow SignalFunction where
  arr f = SF (\a -> (arr f, f a))
  first (SF f) = SF (\(a, b) -> let (sf, x) = f a in (first sf, (x, b)))

delay :: a -> SignalFunction a a
delay x = SF (\a -> (delay a, x))

weirdMultiplier :: SignalFunction (Int, Int) Int
weirdMultiplier = proc (x, y) -> do
  delayed <- delay 1 -< x
  returnA -< delayed * y

runSignalFunction :: SignalFunction a b -> [a] -> [b]
runSignalFunction _ [] = []
runSignalFunction (SF f) (x:xs) = let (sf, y) = f x in y:runSignalFunction sf xs
-}

--------------------------------------- Concurrency

{-
2.2. Используя библиотеку STM, и модули Control.Concurrent, Conctrol.Exception решить задачу "Обедающих философов".
  У нас есть 5 философов, представляемые отдельными потоками, и 5 вилок, которые являются общими данными. Каждый философов может размышлять или обедать, его желания переключаются отправкой сигнала в поток. Если философ хочет есть и рядом с ним есть свободные вилки, он их берёт в руки и ест.
-}

-- TODO: как оцениваем? статически смотрим, что код не приводит к dead/live-lock'ам или просим тесты (можно тесты как отдельное 2.2 задание поставить)

data Fork = ForkAcquired | ForkReleased
data Forks -- = ?

-- | Сигнал переключающий философа
data PhilosopherSignal = Hungry | Full
instance Exception PhilosopherSignal

-- | Любая специфичная для конкретного философа информация
data PhilosopherState -- = ?

-- | Инициализируем философов и возвращаем их хэндлеры.
makePhilosphers :: IO [ThreadId]
makePhilosphers = _

-- | Отправляем сигналы
makeHungry :: ThreadId -> IO ()
makeHungry = _

makeFull :: ThreadId -> IO ()
makeFull = _

-- | Первый аргумент - часть общего состояния. Философ должен обрабатывать исключения типа PhilosopherSignal и переключать своё поведение.
philosopher :: PhilosopherState -> IO ()
philosopher = _
