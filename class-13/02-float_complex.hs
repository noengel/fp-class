import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many)
import Data.Char

{- Напишите парсер для вещественных чисел. -}
float :: Parser Float
float = do (*) <$> minus <*>(positiveFloat <|> fromIntegral <$> natural)
    where
        positiveFloat = do
            a <- natural
            char '.'
            b <- fraction
            if a >= 0 then 
                return $ fromIntegral a + b
            else 
                return $ fromIntegral a - b
                
        minus = (char '-' >> return (-1)) <|> return 1
        
fraction = do
    num <- many digit
    return $ foldr (\ d -> (/ fromIntegral 10) . (+ fromIntegral d)) 0 num

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
    a <- token float
    char ','
    b <- token float
    return $ (a, b)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (token complex<|>float_to_complex) (symbol ";")

float_to_complex = do
    fl <- token float
    return (fl,0)

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ sepBy (token complex<|>float_to_complex) (symbol ",")


