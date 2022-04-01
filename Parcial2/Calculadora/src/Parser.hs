module Parser where


import Infix
import Text.Parsec
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Functor
import Data.Maybe 
import Data.Bifunctor

data Token
    = TkOp Operator
    | Terminal LeafNode
    deriving Show 

data Error = SyntaxError | ParseError | LexerError deriving Show

operators :: Map String Token
operators = Map.fromList $ fmap TkOp <$>
    [ ("+", Plus)
    , ("-", Minus)
    , ("*", Mult)
    , ("/", Div)
    ]

digits :: [Char]
digits = ['0'..'9']

parseInt :: Monad m => ParsecT String u m Token
parseInt = Terminal . LN . read  <$> many1 (oneOf digits)

parseOP :: Monad m => ParsecT String u m Token 
parseOP = anyChar >>= f
    where
        f = maybe (fail "Bad operator Parse") pure . (`Map.lookup` operators) . pure 

parseStr' :: Monad m => ParsecT String u m [Token]
parseStr' = do 
    spaces
    b  <- optionMaybe (void endOfLine <|> eof)
    if isJust b then pure [] else (:) <$> (parseInt <|> parseOP) <*> parseStr'

parseStr :: String -> Either Error [Token]
parseStr = first (const ParseError) <$> parse parseStr' "."