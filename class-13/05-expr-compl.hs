import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad


{-
   Добавьте поддержку вещественных и комплексных чисел из второго упражнения.
   Можете считать, что все числа в выражении являются комплексными (и можете
   не считать, если в состоянии красиво обойтись с типами и всё корректно
   проанализировать).
-}

data Expr = Con Int | Num Float | Complex (Float, Float) | Bin Op Expr Expr
  deriving Show
data Op = Plus | Minus | Mul | Div
  deriving Show

{-
expr   ::= term {addop term}*
term   ::= factor {mulop factor}*
factor ::= nat | '(' expr ')'
addop  ::= '+' | '-'
mulop  ::= '*' | '/'
-}

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
    
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
    a <- token float
    char ','
    b <- token float
    return $ (a, b)

expr = token (term >>= rest addop term)
  where
    rest op unit e1 = optional e1 $ do 
        p <- op
        e2 <- unit
        rest op unit $ Bin p e1 e2
    term = token (factor >>= rest mulop factor)
    factor = token (constant <|> bracket "(" ")" expr)
    addop = binop ("+", Plus) ("-", Minus)
    mulop = binop ("*", Mul) ("/", Div)
    binop (s1, cons1) (s2, cons2) =
          (symbol s1 >> return cons1) <|>
          (symbol s2 >> return cons2)
    intConst = Con `liftM` natural
    floatConst = Num `liftM` float
    complexConst = Complex `liftM` complex
    constant = complexConst <|> floatConst <|> intConst