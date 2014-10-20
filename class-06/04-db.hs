{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}

import System.Environment
import System.IO
import Data.List

splitString :: String -> [String]
splitString str = filter (/= ";") $ groupBy (\x y -> x /= ';' && y /= ';') str

-- Список студентов заданной группы
--showOneGroup :: Int -> Int -> [[String]] -> [[String]]
--showOneGroup course group students = filter (\[_,_,c,g] -> (read c == course) && (read g == group)) students  

-- 1) Вычислить средний возраст студентов заданной группы заданного курса
f1 :: FilePath -> Int -> Int -> IO ()
f1 fname course group = do
	file <- readFile fname
	let 
		students = showOneGroup course group (map splitString (lines file))
	let 
		(avg, count) = foldl (\(av, c) [_,a,_,_] -> (av + read a, c + 1)) (0,0) students
	putStrLn $ show (avg / count)
	where
		showOneGroup course group students = filter (\[_,_,c,g] -> (read c == course) && (read g == group)) students  

-- Сортировка по курсу и группе
sortByGroups :: (Int, Int) -> (Int, Int) -> Ordering
sortByGroups (x,y) (z,t)
	| x < z = LT
	| x > z = GT
	| y < t = LT
	| y > t = GT
	| otherwise = EQ

-- 2) Вычислить количество студентов в каждой группе каждого курса
f2 :: FilePath -> IO ()
f2 fname = do
	file <- readFile fname
	let 
		list = group (sortBy sortByGroups (map (\[_,_,c,g] -> (read c, read g)) (map splitString (lines file))))
	let 
		count = map (\xs -> (head xs, length xs)) list
	putStrLn $ unlines (map (\(x,y) -> show x ++ ": " ++ show y) count)

case' :: Int -> [String] -> IO ()
case' 1 (x1:x2:x3:_) = f1 x1 (read x2) (read x3)
case' 2 (x1:_) = f2 x1
case' 3 (x1:_) = undefined

main = do
	(n:args) <- getArgs
	case' (read n) args