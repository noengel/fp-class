import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}

data Scheme     = FTP | HTTP | HTTPS | Unk String
                    deriving Show
                    
type Login      = String
type Password   = String
type Host       = String
type Port       = String
type Path       = String
type Params     = String
type Anchor     = String

data URL        = URL Scheme (Login, Password) Host Port Path Params Anchor
                    deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

auth = do
    login <- many1 (sat (/= ':'))
    char ':'
    password <- many1 (sat (/= '@'))
    char '@'
    return (login, password)

host = many1 (sat (\x -> x /= ':' && x /= '/'))

port = char ':' >> many1 (sat (\x -> x /= '/' && x /= '?' && x /= '#'))

path = char '/' >> many1 (sat (\x -> x /= '?' && x /= '#'))

params = char '?' >> many1 (sat (/= '#'))

anchor = char '#' >> many (sat $ const True)

url = URL <$> 
    scheme <*> 
    (string "://" >> optional ("", "") auth) <*> 
    host <*> 
    optional "" port <*> 
    optional "" path <*> 
    optional "" params <*> 
    optional "" anchor
      
test_parse = parse url "https://admin:12345@www.internet.com:80/Russia.php?params#anchor"