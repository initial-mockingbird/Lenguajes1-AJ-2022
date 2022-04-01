module Repl where


import Text.Parsec
import Control.Monad (void, forever)
import System.Exit
import Utils 
import Parser 
import Data.Bifunctor
import System.IO (hFlush, stdout)
import Data.List (foldl1')
import Data.Char (toUpper)

data Command 
    = Action Functionality Mode String  
    | Exit
    deriving Show

readCommandGen :: Monad m => String -> (String -> Command) -> ParsecT String u m Command
readCommandGen s f = try (string s) >> spaces >> f <$> manyTill anyChar (void endOfLine <|> eof)

readCalcPre, readCalcPost, readShowPre, readShowPost, readExit :: Monad m => ParsecT String u m Command

readCalcPre  = readCommandGen "PRE"  (Action Eval Pre)
readCalcPost = readCommandGen "POST" (Action Eval Post)
readShowPre  = readCommandGen "PRE"  (Action Print Pre)
readShowPost = readCommandGen "POST" (Action Print Post)
readExit     = string "SALIR" >> (void endOfLine <|> eof) >> pure Exit

executeCommand :: Command -> IO ()
executeCommand Exit = exitSuccess 
executeCommand (Action fc md s) = case mkAction fc md s of
  Left err  -> print err 
  Right str -> putStrLn str

readCommand' :: Monad m => ParsecT String u m Command
readCommand' = spaces >> foldl1' (<|>) 
    [ string "EVAL"    >> spaces >> (readCalcPre <|> readCalcPost)
    , string "MOSTRAR" >> spaces >> (readShowPre <|> readShowPost)
    , readExit 
    ]

readCommand :: String -> Either Error Command
readCommand = first (const LexerError) <$> parse readCommand' "."

repl :: IO ()
repl = forever $ putStr ">>> " >> hFlush stdout >> fmap toUpper <$> getLine  >>= either print executeCommand . readCommand 
