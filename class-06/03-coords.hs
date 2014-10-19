{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Environment
import System.IO
import System.Random
import Data.List

f1 :: FilePath -> IO ()
f1 fname = do
	gen1 <- newStdGen
	gen2 <- newStdGen
	let 
		numb = fst $ randomR (1, 20) gen1 :: Int
		writeFile fname (unlines $ map show $ zipWith (\x y -> (x,y)) (points numb gen1) (points numb gen2))
	where
		points n gen = take n $ randomRs (-100,100) gen :: [Int]  

f2 :: FilePath -> IO ()
f2 fname = do
	file <- readFile fname
	putStrLn $ show $ foldl count [0,0,0,0] $ map (\x -> quarterNum $ read x) (lines file)
	where
		quarterNum (x,y)
			| (x > 0) && (y > 0) = 1
			| (x < 0) && (y > 0) = 2
			| (x < 0) && (y < 0) = 3
			| (x > 0) && (y < 0) = 4
			| otherwise = 0
		count [acc1, acc2, acc3, acc4] x
			| x == 1 = [acc1+1, acc2, acc3, acc4]
			| x == 2 = [acc1, acc2+1, acc3, acc4]
			| x == 3 = [acc1, acc2, acc3+1, acc4]
			| x == 4 = [acc1, acc2, acc3, acc4+1]
			| otherwise = [acc1, acc2, acc3, acc4]

f3 :: FilePath -> IO ()
f3 fname = do
	file <- readFile fname
	let 
		points = map (\x -> read x) $ lines file
		putStrLn $ show $ fst $ foldl (\(pt, max) x -> if (dist x > max) then (x, dist x) else (pt, max)) ((0,0),0) points 
	where
		dist (x,y) = sqrt (x^2 + y^2)

case' args
	| read (head args) == 1 = do
		f1 $ args!!1
	| read (head args) == 2 = do
		f2 $ args!!1
	| read (head args) == 3 = do
		f3 $ args!!1

main = do
  args <- getArgs
  case' args