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
    
load_config :: String -> [Config]
load_config = map (\str -> read_config str) . lines

read_config :: String -> Config
read_config str
    | oper == "summand" = Config Summ $ read $snd $ span ( == '=') oth
    | oper == "multiplier" = Config Mult $ read $ snd $ span ( == '=') oth
    | oper == "divisor" = Config Div $ read $ snd $ span ( == '=') oth
        where (oper, oth) = span (/= '=') str

work :: Int -> Reader [Config] Int
work num = do
  cfg <- ask
  return $ foldl action num cfg
    
action :: Int -> Config -> Int
action a (Config Summ b) = a + b
action a (Config Mult b) = a * b
action a (Config Div b) = a `div` b
  
main = do
    [cfg_file, num_file] <- getArgs
    nums <- readFile num_file
    cfg_data <- readFile cfg_file
    let cfg = load_config cfg_data
    mapM_ (\num -> (print . runReader (work num)) cfg) $ map read $ lines nums