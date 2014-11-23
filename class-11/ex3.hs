{- 3. Пользуясь монадой State, 
      реализовать функции для работы с очередью: enqueue и dequeue. -}
      
import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Control.Monad.State

enqueue :: Int -> State [Int] ()
enqueue x = get >>= \xs -> put $ xs ++ [x]

dequeue :: State [Int] Int
dequeue = get >>= \(x:xs) -> put xs >> return x

test :: State [Int] ()
test = do
    x <- dequeue
    y <- dequeue
    enqueue $ x + y
    enqueue $ x * y
    x <- dequeue
    enqueue $ x * 10

show_test :: [Int] -> [Int]
show_test queue = snd $ runState test queue

run_test = show_test [1,2,3,4,5,6,7] == [4,5,6,7,3,2,30]