{-1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.-}

import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Control.Monad.Reader

data Operation = Summ | Mult | Div
    deriving (Show, Enum)
    
data Config = Config Operation Int
    deriving (Show)
    
summand :: Integer -> Reader Integer Integer
summand x = do
    n <- ask
    return (x + n)

multiplier :: Integer -> Reader Integer Integer
multiplier x = do
    n <- ask
    return (x * n)
    
division :: Integer -> Reader Integer Integer
division x = do
    n <- ask
    return (x `div` n)
    
--subwork :: Reader Config ()
subwork = undefined    

--work :: Reader Config ()
work = undefined
    
load_file :: FilePath -> IO String
load_file file = readFile file
    
load_config :: String -> [Config]
load_config = map (\str -> read_config str) . lines

read_config :: String -> Config
read_config str
    | oper == "summand" = Config Summ $ read $snd $ span ( == '=') oth
    | oper == "multiplier" = Config Mult $ read $ snd $ span ( == '=') oth
    | oper == "division" = Config Div $ read $ snd $ span ( == '=') oth
        where (oper, oth) = span (/= '=') str
        
main = undefined