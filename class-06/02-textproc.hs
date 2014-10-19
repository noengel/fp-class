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
	| read (head args) == 2 = do
		f2 (args!!1) (args!!2) (args!!3)
	| read (head args) == 3 = do
		f3 $ args!!1
	| read (head args) == 4 = do
		f4 (args!!1) (args!!2)
	| read (head args) == 5 = undefined
	
f1 :: FilePath -> IO()
f1 fname = do
	file <- readFile fname
	print $ foldl (\acc x -> if x == '\n' then acc + 1 else acc) 0 file
	
f2 :: FilePath -> String -> String -> IO()
f2 fname wh s = do
	file <- readFile fname
	if wh == "head" then do
		writeFile "tmp.txt" (s ++ "\n" ++ file)
	else do
		writeFile "tmp.txt" (file ++ "\n" ++ s)
	tmpfile <- readFile "tmp.txt"
	writeFile fname tmpfile
	removeFile "tmp.txt"
	
f3 :: FilePath -> IO()
f3 fname = do
	file <- readFile fname
	putStrLn $ map toUpper file
	
f4 :: FilePath -> FilePath -> IO ()
f4 fname1 fname2 = do
  file1 <- readFile fname1
  file2 <- readFile fname2
  writeFile "merge.txt" $ unlines (zipWith (\x y -> x ++ " " ++ y) (lines file1) (lines file2))
		
main = do
	args <- getArgs
	case' args