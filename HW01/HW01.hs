{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module HW01 where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Semigroup
import Data.Foldable
-- Общее для всех задач: все рекурсивные вызовы должны быть хвостовыми.

{-
1. Задача на лямбда-исчисление
1.1. Уберите скобки в следующем лямбда-терме, произвести редукцию. Расписать пошагово:
((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))
1. убираем внешние скобки (4 пары: внешние, при теле функции, при лямбде, сохраняется порядок)
(λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s) 	внешние
(λ p. (λ q. (q (p r)) s)) ((q ((λ p. p) r)) s) 		при теле функции
(λ p. λ q. (q (p r)) s) ((q ((λ p. p) r)) s) 		при лямбде
(λ p. λ q. (q (p r)) s) (q (λ p. p) r s)  			сохраняется порядок
2. f(x) f(y) -> f(x, y)
(λ p q. (q (p r)) s) ((q ((λ p. p) r)) s)
3. 
(λ q. (q (p r)) s) [p = ((q ((λ p. p) r)) s)]		бета редукция
(λ x. (x (p r)) s) [p = ((q ((λ p. p) r)) s)]		замена из-за дублирования буквы
(λ x. (x (((q ((λ p. p) r)) s) r)) s)
(λ x.x (((q ((λ p. p) r)) s) r) s)					убираем скобки
4. 
(λ x.x (((q (p[p = r])) s) r) s)					бета редукция
(λ x.x (((q (r)) s) r) s)							убираем скобки	
(λ x.x ((q r s) r) s)
5. (λ x.x (q r s r) s)								убираем скобки, сохраняется порядок

1.2. Аналогично:
((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]

1. f(x) f(y) -> f(x, y)
((λ a b. (λ x. x) b a (a b x) ((λ a b. a) x)) (λ b. b)) [x := b]
2. бета редукция
((λ a b. (x[x = b]) a (a b x) ((λ ab. a) x)) (λ b. b)) [x := b]
((λ a b. b a (a b x) ((λ ab. a) x)) (λ b. b)) [x := b]
3. бета редукция
((λ a b. b a (a b x) ((λb. a) [a=x])) (λ b. b)) [x := b]
((λ a b. b a (a b x) (λb. x)) (λ b. b)) [x := b]
4. бета редукция (с необходимыми заменами)
((λ b. b a (a b x) (λb. x)) [a = λ b. b]) [x := b]
((λ b. b a (a b x) (λb. x)) [a = λ c. c]) [x := b]
((λ b. b (λ c. c) ((λ c. c) b x) (λb. x))) [x := b]	убираем скобки
(λ b. b (λ c. c) ((λ c. c) b x) (λb. x)) [x := b]
5. бета редукция
(λ b. b (λ c. c) (c[c = b x]) (λb. x)) [x := b]
(λ b. b (λ c. c) (b x) (λb. x)) [x := b]
6. бета редукция (с необходимыми заменами)
(λ b. b (λ c. c) (b x) (λb. x)) [x := b]
(λ a. a (λ c. c) (a x) (λd. x)) [x := b]

(λ a. a (λ c. c) (a b) (λd. b))
-}

{-
2. Реализовать расширенный алгоритм Евклида: `euclid x y = (a, b, r)`, где `a*x + b*y = r`.
-}

euclid :: Integer -> Integer -> (Integer, Integer, Integer)
euclid 0 x = (0, 1, x)
euclid x y = 
	(a, b, d)
	where
		(a1, b1, d) = euclid (y `mod` x) x
		a = b1 - (y `div` x) * a1
		b = a1

test_euclid :: ((Integer, Integer), (Integer, Integer, Integer))-> Integer
test_euclid ((x, y), (a, b, d)) = (a * x + b * y - d)

{-
3. Реализуйте функцию, считающую n-ое число Каталана:
https://en.wikipedia.org/wiki/Catalan_number
-}
factorial :: Integer -> Integer -> Integer
factorial 1 x = x
factorial n x = factorial (n - 1) (x * n)

catalan :: Integer -> Integer
catalan 0 = 1
catalan 1 = 1
catalan n = (factorial (2 * n) 1) `div` (factorial (n) 1) `div` (factorial (n + 1) 1)

{-
4. Не пользуясь стандартными функциями (арифметическими можно), сгенерируйте список всех степеней числа.
pows 2 = [1,2,4,8,...]
-}

pows :: Integer -> [Integer]
pows x = [x ^ i | i <- [0,1..]]

{-
5. Написать функцию, раскладывающую число на список его простых сомножителей.
primesOf 12 = [2,2,3]
-}
step :: Integer-> Integer -> [Integer]
step n int =
	if n == 1 
      	then []
      else 
      	if n `mod` int == 0
      		then int : step (n `div` int) int
      		else
      			step n (int + 1)

primesOf :: Integer -> [Integer]
primesOf 0 = []
primesOf 1 = []
primesOf n = step n 2
      
-- [i | i <- [2,3..x], x >= i, x `mod` i == 0, x >= i]

{- 6. Заселить следующие типы термами: -}

-- # 6.1:

fun1 :: a -> b
fun1 a = undefined

-- OR fun1 a = error "Not defied Type"
-- OR fun1 a = fun1 a

-- # 6.2:

fun2 :: (a -> b -> c) -> (a -> b) -> a -> c
fun2 f g a = f a (g a)
{-
f :: (a -> b -> c)
g :: (a -> b)
a :: a
c = f(a, b) = f(a, g(a)) correct type:)
-}

{-
7. Функции head и tail из библиотеки Data.List не определены на пустых списках.
-}

{-
7.1 Требуется реализовать тип данных "непустой список", который гарантированно не может иметь длину меньше 1. (Можно использовать дополнительный тип при необходимости).
-}
data MyNonEmptyList a =  OneElement a | ListCon a (MyNonEmptyList a)
	deriving (Show)
--Ambiguous occurrence ‘NonEmptyList’ TODO

{-
7.2. Реализовать базовый функционал для типа "непустой список".
-}

lengthNonEmpty :: MyNonEmptyList a -> Int
lengthNonEmpty (OneElement x) = 1
lengthNonEmpty (ListCon x xs)= 1 + lengthNonEmpty xs


headNonEmpty :: MyNonEmptyList a -> a
headNonEmpty (OneElement x) = x
headNonEmpty (ListCon x xs) = x

toListNonEmpty :: MyNonEmptyList a -> [a]
toListNonEmpty (OneElement x) = [x]
toListNonEmpty (ListCon x xs) = x : (toListNonEmpty xs)

append :: MyNonEmptyList a -> MyNonEmptyList a -> MyNonEmptyList a
append (OneElement x) ys = ListCon x ys
append (ListCon x xs) ys = ListCon x (xs `append` ys)

reverseNonEmptyHelp :: MyNonEmptyList a -> MyNonEmptyList a -> MyNonEmptyList a
reverseNonEmptyHelp (OneElement x) xs = ListCon x xs
reverseNonEmptyHelp (ListCon x xs) ys = reverseNonEmptyHelp xs (ListCon x ys)


reverseNonEmpty :: MyNonEmptyList a -> MyNonEmptyList a
reverseNonEmpty (OneElement x) = (OneElement x)
reverseNonEmpty (ListCon x xs) = reverseNonEmptyHelp xs (OneElement x)

{-
7.3. Реализовать базовые инстансы для типа "непустой список".
-}

instance Eq a => Eq (MyNonEmptyList a) where
	(==) (OneElement x) (OneElement y) = (x == y)
	(==) (ListCon x xs) (ListCon y ys) = (x == y) && (xs==ys)
	(==) _ _ = False

instance Semigroup (MyNonEmptyList a) where
	(<>) (OneElement a) b = ListCon a b
	(<>) (ListCon a  as) b = ListCon a (as `append` b)

{-
8. Определим тип "непустое дерево":
-}

data Tree a = Node a [Tree a]    deriving (Show, Eq)

-- -- # 8.1 Реализовать instance Functor для деревьев

instance Functor Tree where
 fmap :: (a -> b) -> Tree a -> Tree b
 fmap f (Node a []) = Node (f a) []
 fmap f (Node a xs) = Node (f a)  (map (fmap f) xs)

-- # 8.2. Реализовать функцию, которая возвращает список элементов дерева

treeToList :: Tree a -> [a]
treeToList (Node x xs) = x : concatMap treeToList xs

-- -- # 8.3 Реализовать проверку что дерево является кучей, то есть голова любого узла меньше его потомков.
getNode :: Tree a -> a
getNode (Node a xs) = a

getNodes :: Tree a -> [a]
getNodes (Node a xs) = (map getNode xs)

rootLess :: Ord a => Tree a -> Bool
rootLess (Node a xs) = all (>a) (getNodes (Node a xs))

isHeap :: Ord a => Tree a -> Bool
isHeap (Node a xs) = all (== True) (map rootLess xs) && (rootLess (Node a xs))

--  # 8.4 Назовём кучу "потрясной", если для любого узла все элементы k-го поддерева не превосходят любой элементы k+1-го поддерева. Реализуйте проверку на "потрясность".
-- Например:
--                 0
--                / \
--               1   5
--              / \
--             2   6
--  У узла 0 есть два поддерева - с корнем 1 и 5. Элемент 6 из поддерева Т_1 больше элемента 5 из поддерева Т_2, то есть эта куча не "потрясная". В целом, это похоже на B-дерево, если бы у него не было ограничений на количество элементов в массивах.

{-
 Заметим, что в таком случае (и только в таком) просотй обход в глубину должен давать отсортированный массив.
 1) Если массив получился отсортированным, то тогда по алгоритму обхода в глубину соблюдается условие потрясности.
 2) Пусть массив получился неотсортированным, тогде мы зашли в вершину x раньше y, |x| > |y|. 
 Но в таком случае найдем их ближайшего общего предка, и тогда для его поддеревьев не будет соблюдатся условие потрясности.
(Так и рабоает q-sort)
Заметим также, что обход в глубину реализован в функции treeToList.
-}
sorted ::  Ord a =>  [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

isAmazingHeap :: Ord a => Tree a -> Bool
isAmazingHeap a = isHeap a && sorted (treeToList a)

{-
9. В стандартной библиотеке Хаскелля тип Int не является экземпляром классов Monoid и Semigroup, потому что целые числа являются моноидом по сложению и умножению одновременно.
-}

{-
9.1. Напишите типы-обёртки SumMonoid, ProdMonoid и соответствующие инстансы.
-}

data SumMonoid a = SumMonoid a

data ProdMonoid a = ProdMonoid a

instance Num a => Semigroup (SumMonoid a) where
	(<>) (SumMonoid a) (SumMonoid b) = (SumMonoid (a + b))

instance Num a => Monoid (SumMonoid a) where
	mempty = SumMonoid 0
	mappend = (<>)
instance Num a => Semigroup (ProdMonoid a) where
	(<>) (ProdMonoid a) (ProdMonoid b) = (ProdMonoid (a * b))

instance Num a => Monoid (ProdMonoid a) where
	mempty = ProdMonoid 1
	mappend = (<>)

-- {-
-- 9.2. Используя метод `foldMap` из библиотеки Data.Foldable написать функции, суммирующие и перемножающие список целых чисел.
-- -}

sumAll :: [Int] -> Int
sumAll listInt= (\(SumMonoid a) -> a) (foldMap SumMonoid listInt)

prodAll :: [Int] -> Int
prodAll listInt = (\(ProdMonoid a) -> a) (foldMap ProdMonoid listInt)

{- 10. Lambda lifting - превращение свободных переменных в аргументы. Например:

```
pow :: Int -> Int -> Int
pow n = powTail 1
  where
  powTail acc 0 = acc
  powTail acc k = powTail (acc * n) (k - 1)
```

Результат вызова `powTail` зависит от `n`, поэтому мы не можем вынести `powTail` в таком виде наружу.
-}

-- # 10.1. Вынести `powTail` как глобальную функцию.

pow :: Int -> Int -> Int
pow n x = powTail n 1 x

powTail :: Int -> Int -> Int -> Int
powTail n acc 0 = acc
powTail n acc k = powTail n (acc * n) (k - 1)

{- # 10.2
Сделать lambda lifting для `from0` и `from1` и вынести их как глобальные функции.

cyclic :: [Int]
cyclic = let from0 = 0:from1
             from1 = 1:from0
          in from0
-}

cyclic :: [Int]
cyclic = from0 (from1 cyclic)

from0 :: [Int] -> [Int]
from0 list = 0:list

from1 :: [Int] -> [Int]
from1 list = 1:list

--------------------------------------------
main :: IO ()
main = hspec $ do
	describe "Euclid" $ do
	    it "eq" $ do
	      test_euclid ((5, 0), (euclid 5 0)) `shouldBe` 0
	      test_euclid ((0, 5), (euclid 0 5)) `shouldBe` 0
	      test_euclid ((1, 5), (euclid 1 5)) `shouldBe` 0
	      test_euclid ((2, 4), (euclid 2 4)) `shouldBe` 0
	      test_euclid ((15, 25), (euclid 15 25)) `shouldBe` 0
	      test_euclid ((13, 17), (euclid 13 17)) `shouldBe` 0
	      test_euclid ((13, 13), (euclid 13 13)) `shouldBe` 0
  	describe "Catalan" $ do
	    it "factorial" $ do
			factorial 3 1 `shouldBe` 6
			factorial 1 1 `shouldBe` 1
			factorial 5 1 `shouldBe` 120
	    it "catalan" $ do
	    	catalan 1 `shouldBe` 1
	    	catalan 2 `shouldBe` 2
	    	catalan 3 `shouldBe` 5
	    	catalan 6 `shouldBe` 132
	    	catalan 8 `shouldBe` 1430
   	describe "Pow" $ do
   		it "lists" $ do
   			take 4 (pows 2) `shouldBe` [1, 2, 4, 8]
   			take 5 (pows 3) `shouldBe` [1, 3, 9, 27, 81]
   			take 3 (pows 5) `shouldBe` [1, 5, 25]
   	describe "PrimeTime" $ do
   		it "paupau" $ do
   			primesOf 1 `shouldBe` []
   			primesOf 0 `shouldBe` []
   			primesOf 24 `shouldBe` [2, 2, 2, 3]
   			primesOf 25 `shouldBe` [5, 5]
   			primesOf 26 `shouldBe` [2,13]
   			primesOf 27 `shouldBe` [3, 3, 3]
   	describe "No empty" $ do
   		it "length" $ do
   			lengthNonEmpty (OneElement 1) `shouldBe` 1
   			lengthNonEmpty (ListCon 1 (OneElement 2)) `shouldBe` 2
   			lengthNonEmpty (ListCon 1 (ListCon 2 (OneElement 3))) `shouldBe` 3
   		it "head" $ do
   			headNonEmpty (OneElement 1) `shouldBe` 1
   			headNonEmpty (ListCon 2 (OneElement 1)) `shouldBe` 2
   			headNonEmpty (ListCon 3 (ListCon 2 (OneElement 1))) `shouldBe` 3
   		it "list" $ do
   			toListNonEmpty (OneElement 1) `shouldBe` [1]
   			toListNonEmpty (ListCon 2 (OneElement 1)) `shouldBe` [2,1]
   			toListNonEmpty (ListCon 3 (ListCon 2 (OneElement 1))) `shouldBe` [3,2,1]
   		it "reverse" $ do
   			toListNonEmpty (reverseNonEmpty (OneElement 1)) `shouldBe` [1]
   			toListNonEmpty (reverseNonEmpty (ListCon 2 (OneElement 1))) `shouldBe` [1,2]
   			toListNonEmpty (reverseNonEmpty (ListCon 3 (ListCon 2 (OneElement 1)))) `shouldBe` [1,2,3]
   		it "eq" $ do
   			(OneElement 1) == (OneElement 1) `shouldBe` True
   			(ListCon 2 (OneElement 1)) == (ListCon 2 (OneElement 1)) `shouldBe` True
   			(ListCon 3 (ListCon 2 (OneElement 1))) == (ListCon 3 (ListCon 2 (OneElement 1))) `shouldBe` True
   			(ListCon 3 (ListCon 2 (OneElement 1))) == (ListCon 2 (OneElement 1)) `shouldBe` False
   			(OneElement 1) == (OneElement 2) `shouldBe` False
   			(ListCon 2 (OneElement 1)) == (ListCon 1 (OneElement 2)) `shouldBe` False
   			(ListCon 3 (ListCon 2 (OneElement 1))) == (ListCon 3 (OneElement 2)) `shouldBe` False
   			(ListCon 3 (ListCon 2 (OneElement 1))) == (ListCon 2 (ListCon 2 (OneElement 1))) `shouldBe` False
   		it "eq" $ do
   			(OneElement 1) <> (OneElement 2) `shouldBe` (ListCon 1 (OneElement 2))
   			(ListCon 1 (OneElement 2)) <> (OneElement 2) `shouldBe` (ListCon 1 (ListCon 2 (OneElement 2)))
   			(OneElement 2) <> (ListCon 1 (OneElement 2)) `shouldBe` (ListCon 2 (ListCon 1 (OneElement 2)))
   			(ListCon 1 (OneElement 2)) <> (ListCon 3 (OneElement 4)) `shouldBe` (ListCon 1 (ListCon 2 (ListCon 3 (OneElement 4))))
   	describe "Tree" $ do
			it "data" $ do
				Node 5 []  `shouldBe` Node 5 []
				Node 5 [Node 4 []]  `shouldBe` Node 5 [Node 4 []]
				Node 5 [Node 4 [], Node 3 []]  `shouldBe` Node 5 [Node 4 [], Node 3 []]
				Node 4 [Node 3 [Node 1 []], Node 2 []] `shouldBe` Node 4 [Node 3 [Node 1 []], Node 2 []]
			it "fmap" $ do
				fmap (subtract 1) (Node 5 [])  `shouldBe` Node 4 []
				fmap (subtract 1) (Node 5 [Node 4 []])  `shouldBe` Node 4 [Node 3 []]
				fmap (subtract 1) (Node 5 [Node 4 [Node 2 []], Node 3 []])  `shouldBe` Node 4 [Node 3 [Node 1 []], Node 2 []]
			it "treeToList" $ do
				treeToList (Node 5 []) `shouldBe` [5]
				treeToList (Node 5 [Node 4 []]) `shouldBe` [5, 4]
			it "Heap" $ do
				getNodes (Node 5 [Node 4 [Node 2 []], Node 3 []]) `shouldBe` [4, 3]
				rootLess (Node 1 [Node 4 [Node 2 []], Node 3 []]) `shouldBe` True
				rootLess (Node 3 [Node 4 [Node 2 []], Node 3 []]) `shouldBe` False
				isHeap  (Node 0 []) `shouldBe` True
--                 0
--                / \
--               1   5
--              / \
--             2   6
				isHeap  (Node 0 [Node 1 [Node 2 [], Node 6 []], Node 5 []]) `shouldBe` True
--                 1
--                / \
--               1   5
--              / \
--             2   6
				isHeap  (Node 1 [Node 1 [Node 2 [], Node 6 []], Node 5 []]) `shouldBe` False
--                 0
--                / \
--               2   5
--              / \
--             3   1
				isHeap  (Node 0 [Node 2 [Node 3 [], Node 1 []], Node 5 []]) `shouldBe` False
--                 0
--                / \
--               2   10
--              / \    \
--             3   4   11
				isHeap  (Node 0 [Node 2 [Node 3 [], Node 4 []], Node 10 [Node 11 []]]) `shouldBe` True
			it "isAmazingHeap" $ do
				isAmazingHeap  (Node 0 [Node 1 [Node 2 [], Node 6 []], Node 5 []]) `shouldBe` False
				isAmazingHeap  (Node 0 [Node 1 [Node 2 [], Node 4 []], Node 5 []]) `shouldBe` True
				isAmazingHeap  (Node 0 []) `shouldBe` True
				isAmazingHeap  (Node 0 [Node 2 [Node 3 [], Node 2 []], Node 5 []]) `shouldBe` False
				isAmazingHeap  (Node 0 [Node 2 [Node 3 [], Node 4 []], Node 10 [Node 11 []]]) `shouldBe` True
   	describe "Monoid" $ do
			it "Sum" $ do
				sumAll [1,2..10] `shouldBe` 55
				sumAll [1,2,3] `shouldBe` 6
				sumAll [1,2] `shouldBe` 3
				sumAll [1] `shouldBe` 1
				sumAll [] `shouldBe` 0
			it "Prod" $ do
				prodAll [1,2..10] `shouldBe` 3628800
				prodAll [1,2,3] `shouldBe` 6
				prodAll [1,2] `shouldBe` 2
				prodAll [1] `shouldBe` 1
				prodAll [] `shouldBe` 1
	describe "Lambda lifting" $ do
		it "Pow" $ do
			pow 2 2 `shouldBe` 4
			pow 2 0 `shouldBe` 1
			pow 2 5 `shouldBe` 32
			pow 5 2 `shouldBe` 25
			pow 0 0 `shouldBe` 1
			pow 0 2 `shouldBe` 0
			pow 2 10 `shouldBe` 1024
		it "cyclic" $ do
			take 5 cyclic `shouldBe` [0,1,0,1,0]

