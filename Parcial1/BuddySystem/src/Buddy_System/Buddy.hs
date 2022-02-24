{-# LANGUAGE FlexibleContexts #-}
module Buddy_System.Buddy (runSystem) where

import Buddy_System.Buddy_Internal.Buddy

import Control.Monad.Writer.Strict ( MonadIO(..) )
import Control.Monad.State.Strict ( MonadState(get), StateT, execStateT )
import Control.Monad.Except 
import Data.Char (toLower)
import Text.Read (readMaybe)
import System.Exit
import Control.Monad 
errorMessages :: Errors -> String
errorMessages InsufficientMemoryE  = "Insufficient memory! The block size is just too big :("
errorMessages FragmentationE       = "Fragmentation Error! The memory is too fragmented to fit the block"
errorMessages AlreadyExecutingE    = "Already Executing Error! cannot allocate a block that's already in memory"
errorMessages NotInMemoryE         = "Not in Memory Error! cannot delete what's not in there"


handler :: MonadIO m => Errors -> m ()
handler = liftIO . putStrLn . errorMessages

mallocHandled :: (MonadError Errors m, MonadIO m) => Nombre -> Int -> StateT MemState m ()
mallocHandled n s = malloc n s `catchError` handler 

freeHandled :: (MonadError Errors m, MonadIO m) => Nombre -> StateT MemState m ()
freeHandled n = free n `catchError`  handler

-- I TOTALLY FORGOT THAT THE MONAD ERROR OF IO IS UNIQUELY DETERMINED BY IOEXCEPTION
-- I should find a way to separate the concerns of reading/printing from IO with the actual processing
-- but until then, ExceptT over IO shall do.
repl ::  StateT MemState (ExceptT Errors IO ) ()
repl = do
    liftIO $ putStr ">>> "
    input <- words <$> liftIO getLine 

    case input of
        [] -> repl
        ["RESERVAR",nombre,cantidad] -> case readMaybe cantidad of 
            Just n -> mallocHandled nombre n >> repl 
            _      -> liftIO (putStrLn "RESERVAR <nombre> <cantidad>: <cantidad> debe ser un entero!") >> repl
        ["LIBERAR",nombre] -> freeHandled nombre >> repl
        ["MOSTRAR"] -> get >>= \s -> liftIO (putStrLn $ showMemory s) >> repl
        ["SALIR"]   -> liftIO (putStrLn "Gracias :)" >> exitSuccess)
        _ -> liftIO (putStrLn "Comando invalido!") >> repl

    return ()

runSystem :: Int -> IO ()
runSystem = void . runExceptT . execStateT repl . iMST 