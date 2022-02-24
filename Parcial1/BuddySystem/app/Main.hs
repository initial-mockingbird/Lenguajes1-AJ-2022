module Main where

import Buddy_System.Buddy
import System.Environment
import System.IO
import Text.Read (readMaybe)
main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    arg   <- fmap readMaybe <$> getArgs
    case arg of
        [Just n] -> runSystem n
        _        -> error "Error! Se debe proveer un unico parametro de tipo entero."
    
