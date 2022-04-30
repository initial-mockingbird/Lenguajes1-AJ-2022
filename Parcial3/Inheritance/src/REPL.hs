{-# LANGUAGE  FlexibleContexts  #-}
module REPL where 

import Lexer
import Controller 
import System.Exit 
import Data.Bifunctor 
import System.IO 
import Data.List
import Data.Char 
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad 
import Text.Parsec

data CommandErr

execCommand :: (Monad m, MonadIO m, MonadError MError m) => Command -> StateT InnerSt m ()
execCommand Describir {objective = Tipo t} = do 
    (describe (CID t) >>= liftIO . putStr . prettyShowMST)  `catchError` handler
    where 
        handler InexistentClass  = liftIO (putStrLn $ "ERROR, el simbolo "++ show t ++ " no esta definido")
        handler e                = throwError e
execCommand ClassDef {base = Tipo c, supers = sps, ms=m} 
    = insertClass (CID c) (CID . getTipo <$> sps) (MID . getNombre <$> m)   `catchError` handler
    where 
        handler (InexistentSuper s) = liftIO (putStrLn $ "ERROR, no existe la clase super: "++ s)
        handler RepeatedDefinitions = liftIO (putStrLn   "ERROR, definiciones repetidas")
        handler Cycle               = liftIO (putStrLn   "ERROR, Ciclo generado")
        handler AlreadyExists       = liftIO (putStrLn   "ERROR, la clase ya existe")
        handler e                = liftIO (putStrLn "MAXIERROR AAA") >> throwError e
execCommand Salir = liftIO exitSuccess


-- | Trata de parsear un commando, retornando `CommandErr` en caso de error.
readCommand :: String -> Either CommandErr Command
readCommand = first (const (undefined :: CommandErr)) <$> parse parseEntry "."

-- | logica del REPL. 
repl' :: StateT InnerSt (ExceptT MError IO) b
repl' 
    = forever 
    $ liftIO (putStr ">>> " >> hFlush stdout >> toUpperCommand <$> getLine) 
    >>= either (const $ liftIO (putStrLn "ERROR, comando invalido")) execCommand . readCommand 
    where
        toUpperCommand xs = case words xs of
            (command:xs) -> unwords (map toUpper command : xs)
            []           -> []

-- | REPL.
repl :: IO ()
repl = void $ runExceptT $ execStateT repl' initialST