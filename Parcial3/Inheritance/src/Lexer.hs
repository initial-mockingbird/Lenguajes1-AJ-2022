module Lexer where

import Text.Parsec
import Text.Parsec.Char 
import Data.Maybe (fromMaybe)
newtype Tipo   = Tipo  {getTipo :: String}
newtype Nombre = Nombre {getNombre :: String}

data Command 
    = ClassDef {base :: Tipo, supers :: [Tipo], ms :: [Nombre]}
    | Describir {objective :: Tipo}
    | Salir 


parseIdentifier :: Monad m => ParsecT String u m String
parseIdentifier = (:) <$> letter <*> many (letter <|> digit <|> char '_')

parseClassDef :: Monad m => ParsecT String u m Command
parseClassDef 
    = string "CLASS"
    >> spaces 
    >> ClassDef 
    <$> (Tipo <$> parseIdentifier <* spaces)
    <*> (fromMaybe [] <$> optionMaybe parseSupers)
    <*> (fmap Nombre <$> many1 (parseIdentifier <* spaces))

    where
        f :: Monad m => ParsecT String u m [String]
        f     = many1 (parseIdentifier <* spaces)

        parseSupers :: Monad m => ParsecT String u m [Tipo]
        parseSupers 
            = char ':' 
            >> spaces 
            >> fmap Tipo <$>  (( (:[]) <$> parseIdentifier) <|> (char '{' >> spaces >> (f  <* char '}'))) <* spaces

parseDescribir :: Monad m => ParsecT String u m Command
parseDescribir 
    = string "DESCRIBIR"
    >> spaces 
    >> Describir . Tipo 
    <$> parseIdentifier

parseSalir :: Monad m => ParsecT String u m Command
parseSalir = string "SALIR" >> pure Salir


parseEntry :: Monad m => ParsecT String u m Command
parseEntry = spaces >> (parseClassDef <|> parseDescribir <|> parseSalir)