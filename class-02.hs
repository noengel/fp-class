-- Проверка Windows-клиента для GitHub'a
-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms t = (h, m, s)
	where 
		h = div t 3600
		m = div (mod t 3600) 60
		s = mod t 60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s


-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

triangle :: Point -> Point -> Point -> (Double, Double)
triangle a b c = (p, s)
  where
	dc = distance a b
	da = distance b c
	db = distance c a
	p = da + db + dc
	sp = p / 2
	s = sqrt $ sp * (sp - da) * (sp - db) * (sp - dc)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
	| mod x 2 == 0 = 1 + nEven xs
	| otherwise = nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems (x:xs) = x*2 : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
	| mod x 2 == 1 = x : fltOdd xs
	| otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delAllNeg :: Integral a => [a] -> [a]
delAllNeg [] = []
delAllNeg (x:xs)
	| x >= 0 = x : delAllNeg xs
	| otherwise = delAllNeg xs

-- б) увеличить элементы с чётными значениями в два раза;
twiceEven :: Integral a => [a] -> [a]
twiceEven [] = []
twiceEven (x:xs) 
	| mod x 2 == 0 = x*2 : twiceEven xs
	| otherwise = x : twiceEven xs
	
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
swapOddAndEven :: Integral a => [a] -> [a]
swapOddAndEven [] = []
swapOddAndEven (x:y:xs) = y : x : swapOddAndEven xs
swapOddAndEven (x) = []

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x + y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_pair :: [Integer] -> [Integer] -> [(Integer, Integer)]
combine_pair [] ys = []
combine_pair xs [] = []
combine_pair (x:xs) (y:ys) = (x, y) : combine_pair xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
getRevFirstN :: Integer -> [Integer]
getRevFirstN n 
	| n == 1 = [n]
	| otherwise = n : getRevFirstN (n-1)
		
-- б) в порядке возрастания.
getFirstN :: Integer -> [Integer]
getFirstN n = [1..n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insertElem :: a -> [a] -> [a]
insertElem x (x1:x2:xs) = x1 : x : insertElem x (x2:xs)
insertElem x l = l

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
split :: Eq a => [a] -> ([a], [a])
split (x:xs) = split' ([x], xs)
    where
        split' ((x:xs), (y:ys))
            | x == y = split' (x:xs ++ [y], ys)
            | otherwise = (x:xs, y:ys)
			
--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a 
-- доступ по индексу
getElem :: [a] -> Int -> a
getElem (x:xs) 0 = x 
getElem (x:xs) i = getElem xs (i-1)

-- б) Eq a => [a] -> a -> Bool
-- проверка вхождения элемента
isContains :: Eq a => [a] -> a -> Bool
isContains [] a = False
isContains (x:xs) a
    | a == x = True
    | otherwise = isContains xs a
	
-- в) [a] -> Int -> [a]
-- удалить элемент по индексу
delElem :: [a] -> Int -> [a]
delElem (x:xs) 0 = xs
delElem (x:xs) i = x : delElem xs (i-1)

-- г) a -> Int -> [a]
-- список из k штук элементов a
aRepeat :: a -> Int -> [a]
aRepeat a 0 = [a]
aRepeat a n = a : aRepeat a (n-1)

-- д) [a] -> [a] -> [a]
-- склеить два списка
listConcat :: [a] -> [a] -> [a]
listConcat [] [] = []
listConcat [] (y:ys) = y : listConcat [] ys
listConcat (x:xs) y = x : listConcat xs y

-- е) Eq a => [a] -> [[a]]
--
-- ж) [a] -> [(Int, a)]
--
-- з) Eq a => [a] -> [a]
--
