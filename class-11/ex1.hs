{-1. �������� ���������, ������ ������� ����������� ���������������� ������, ���������� ������ ���������� �������:
��� ����=��������
���������� ������� ����� �������� summand (���������), multiplier (���������), divisor (��������). ��� ��������
�������� ������ �������. � �������� ���������� ��������� ������ ��������� �������� ��� ����������������� �����
� ��� ���������� ����� � �������������� �������. ��� ������ ����� ������ �� ������� ����� ����������� ��������,
��������� � ���������������� �����, �� ���� ����� ������������, ���������� � ������� ��������������.
���� �����-���� ���� �����������, �� �������� �� �����������. ���������� ���������� ��������� �� �������.
������������ ������ � ���������� ����������������� ����� ���������� ������ Reader.-}

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