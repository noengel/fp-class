{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Environment
import System.IO
import System.Directory
import Data.List
import Data.Char

case' args
	| read (head args) == 1 = do
		f1 $ args!!1
	| read (head args) == 2 = 
		f2 (args!!1) (args!!2) (args!!3)
	| read (head args) == 3 = do
		f3 $ args!!1
	| read (head args) == 4 = undefined
	| read (head args) == 5 = undefined
	
f1 :: String -> IO()
f1 fname = do
	file <- readFile fname
	print $ foldl (\acc x -> if x == '\n' then acc + 1 else acc) 0 file
	
f2 :: String -> String -> String -> IO()
f2 fname wh s = do
	file <- readFile fname
	if wh == "head" then do
		writeFile "tmp.txt" (s ++ "\n" ++ file)
	else do
		writeFile "tmp.txt" (file ++ "\n" ++ s)
	tmpfile <- readFile "tmp.txt"
	writeFile fname tmpfile
	removeFile "tmp.txt"
	
f3 :: String -> IO()
f3 fname = do
	file <- readFile fname
	print $ map toUpper file
		
main = do
	args <- getArgs
	case' args