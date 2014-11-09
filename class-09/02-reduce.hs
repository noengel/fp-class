import System.Environment
import Data.Maybe
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a 
    | a `mod` 3 == 0 = 0
    | odd a = a^2
    | otherwise = a^3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n fa = foldl (\acc _ -> fmap reduce acc) fa [1..n]

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = foldr (\(x,y) acc -> (x * y) : acc) []

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe [(n,x)] = Just x
toMaybe xs = Just (minimum $ toList xs)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Empty list"
toEither [(n,x)] = Right x
toEither xs = Right (minimum $ toList xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: (Random a, Integral a) => [(a, a)]  -> IO a
toIO xs = do
    gen <- newStdGen
    let x = fst $ randomR (1, 1000) gen
    return $ max x (minimum $ toList xs)

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs str = (head str, read $ last str)

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
    file <- readFile fname
    return $ foldr (\x acc -> (parse x) : acc) [] $ lines file 
    where
        parse x = let l = words x in (read $ head l, read $ last l)

main = do
    (fname, n) <- parseArgs `fmap` getArgs
    ps <- readData fname
    print $ reduceNF n (toList ps)
    print $ reduceNF n (toMaybe ps)
    print $ reduceNF n (toEither ps)
    reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
 
    :main 02.txt 0
    [16668,480,48256,0,-256,-46912,3136]
    Just (-46912)
    Right (-46912)
    48256

-}