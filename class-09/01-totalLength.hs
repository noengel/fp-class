import System.Environment

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength = foldl (\acc x -> acc + length x) 0

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 _ 0 = Nothing
build1 c n = Just $ take n $ iterate (c:) [c]

{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 c n
    | n == 0 = Left "n = 0"
    | n > 100 = Left "n > 100"
    | c =='x' = Left "Роспотребнадзор запрещает создавать строки из символа 'x'"
    | otherwise = Right $ take n $ iterate (c:) [c]

{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}

main = do
    (fname : c : n : xs) <- getArgs
    res1 <- fmap totalLength getArgs
    putStrLn $ "Длина аргументов командной строки: " ++ show res1 
    
    res2 <- fmap totalLength $ fmap words $ readFile fname
    putStrLn $ "Длина текста: " ++ show res2 
    
    putStrLn $ "build1: " ++ show (fmap totalLength $ build1 (read c) (read n))
    
    putStrLn $ "build2: " ++ show (fmap totalLength $ build2 (head c) (read n))