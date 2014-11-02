import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SequenceQueue as SQ
import System.Random
import System.Environment
import System.IO

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
checkQueue q = lastElem (enqueue q 5) == 5
 where
  lastElem q = let (x, q') = dequeue q in
               if isEmpty q' then x else lastElem q'

-- Генерация списка из n случайных целых чисел
randList :: Int -> IO [Int]
randList n = do
    gen <- newStdGen
    return (take n $ randomRs(1, 1000) gen)

-- Добавляем в очередь n штук элементов и извлекаем n-1 элемент
func :: (AbstractQueue a) => a Int -> [Int] -> a Int
func q l = nDeq (length l - 1) $ foldl (enqueue) q l
    where
        nDeq n qu 
            | n == 0 = qu
            | otherwise = nDeq (n-1) (snd $ dequeue qu)

-- Добавляем и извлекаем элементы n раз
push_pop :: (AbstractQueue a) => a Int -> [Int] -> Int -> a Int
push_pop q l n = push_pop' q l 1 n
    where 
        push_pop' qu li i k
            | k == 1 = func q l
            | i == 1 = push_pop' (enqueue qu $ head li) (tail li) 2 k 
            | i == n = func qu li
            | otherwise = push_pop' (func qu li) (drop i li) (i + 1) k 

queueToList :: (AbstractQueue a) => a Int -> [Int]
queueToList q
    | isEmpty q = []
    | otherwise = let (x, qu) = dequeue q in x : queueToList qu

lastElem :: (AbstractQueue a) => a Int -> [Int] -> Int -> IO [Int]
lastElem q l n = return $ queueToList (push_pop q l n)

readN n
    | n == 1 = 1
    | otherwise = n + readN (n-1)

main = do
    args <- getArgs
    n <- readIO $ head args
    list <- randList $ readN n
    t1 <- lastElem (empty :: Q.Queue Int) list n
    t2 <- lastElem (empty :: FQ.Queue Int) list n
    t3 <- lastElem (empty :: SQ.Queue Int) list n
    putStrLn $ show (t1 == t2 && t2 == t3 && length t1 == n && length t2 == n && length t3 == n)
