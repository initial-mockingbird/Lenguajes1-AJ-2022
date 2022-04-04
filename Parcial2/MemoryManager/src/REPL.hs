{-# LANGUAGE FlexibleContexts #-}

module REPL where


import Text.Parsec
    ( alphaNum,
      endOfLine,
      space,
      spaces,
      string,
      eof,
      many1,
      (<|>),
      parse,
      try,
      ParsecT )
import System.Exit ( exitSuccess )
import Data.Bifunctor ( Bifunctor(first) )
import System.IO (hFlush, stdout)
import Data.List (foldl1')
import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Strict
    ( StateT, MonadIO(..), forever, void, execStateT )
import Control.Monad.Except
    ( MonadError(..), MonadIO(..), forever, void, runExceptT, ExceptT )
import Control.Monad ( forever, void ) 
import Definitions
    ( querySymbol,
      Error(AlreadyFreed, SymbolNotDefined),
      SymbolTable,
      malloc,
      free,
      assign,
      initialST )


-- | Representa un identificador.
newtype Identificador = Identificador {getID :: String}    deriving (Eq, Ord, Show)
-- | Representa un Valor.
newtype Valor         = Valor         {getValor :: String} deriving (Eq, Ord, Show)

-- | Posibles comandos.
data Command 
    = Reservar Identificador Valor
    | Asignar  Identificador Identificador
    | Liberar  Identificador
    | Imprimir Identificador
    | Exit
    deriving Show

-- | Representa un error en el comando
data CommandErr

-- | Parsea 1 o mas espacios.
spaces1 :: Monad m => ParsecT String state m ()
spaces1 = space >> spaces

-- | Parsea el comando reservar.
readReservar :: Monad m =>  ParsecT String u m Command
readReservar 
  =   try (string "RESERVAR") >> spaces1
  >>  Reservar <$> (Identificador <$> many1 alphaNum)
  <*> (spaces >> Valor <$> many1 alphaNum <* (spaces >> eof))

-- | Parsea el comando asignar.
readAsignar :: Monad m =>  ParsecT String u m Command
readAsignar
  =   try (string "ASIGNAR") >> spaces1 
  >>  Asignar <$> (Identificador <$> many1 alphaNum)
  <*> (spaces >> Identificador <$> many1 alphaNum <* (spaces >> eof))

-- | Parsea el comando liberar.
readLiberar :: Monad m =>  ParsecT String u m Command
readLiberar
  =   try (string "LIBERAR") >> spaces1 
  >>  Liberar <$> (Identificador <$> many1 alphaNum) <* eof

-- | Parsea el comando imprimir.
readImprimir :: Monad m =>  ParsecT String u m Command
readImprimir
  =   try (string "IMPRIMIR") >> spaces1 
  >>  Imprimir <$> (Identificador <$> many1 alphaNum) <* eof
  
-- | Parsea el comando salir.
readExit :: Monad m => ParsecT String u m Command
readExit = string "SALIR" >> (void endOfLine <|> eof) >> pure Exit

-- | Parsea cualquier comando.
readCommand' :: Monad m => ParsecT String u m Command
readCommand' = spaces >> foldl1' (<|>) 
    [ readReservar
    , readAsignar
    , readLiberar
    , readImprimir
    , readExit 
    ]

-- | Dado un comando, lo ejecuta
execCommand :: (Monad m, MonadIO m, MonadError Error m) => Command -> StateT (SymbolTable Identificador Valor) m ()
execCommand (Reservar id@(Identificador _id) val@(Valor _val)) = mallocAction
    where
        mallocAction 
            =  malloc id val 
            >> liftIO (putStrLn $ "Se reservo " ++ show _id ++ " con el valor: " ++ show _val)
execCommand (Asignar id1@(Identificador _id1) id2@(Identificador _id2)) = assignAction `catchError` handler 
    where
        handler SymbolNotDefined = liftIO (putStrLn $ "ERROR, el simbolo "++ show _id2 ++ " no esta definido")
        handler e = throwError e

        assignAction 
            =  assign id1 id2 
            >> liftIO (putStrLn $ "Se asigno el valor de " ++ show _id2 ++ " a: " ++ show _id1)
execCommand (Liberar id@(Identificador _id)) = freeAction `catchError` handler 
    where
        freeAction 
            =  free id 
            >> liftIO (putStrLn $ "Se libero: " ++ show _id)
        
        handler SymbolNotDefined = liftIO (putStrLn $ "ERROR, el simbolo "++ show _id ++ " no esta definido")
        handler AlreadyFreed     = liftIO (putStrLn $ "ERROR, el simbolo "++ show _id ++ " ya ha sido liberado previamente")
        handler e                = throwError e
execCommand (Imprimir id@(Identificador _id)) = imprimirAction `catchError` handler
    where
        imprimirAction = querySymbol id >>= (\(Valor val) -> liftIO (putStrLn $ "El valor de: " ++ show _id ++ " es: " ++ val)) 

        handler SymbolNotDefined = liftIO (putStrLn $ "ERROR, el simbolo "++ show _id ++ " no esta definido")
        handler AlreadyFreed     = liftIO (putStrLn $ "ERROR, el simbolo "++ show _id ++ " ya ha sido liberado previamente")
        handler e                = throwError e 

execCommand Exit = liftIO exitSuccess

-- | Trata de parsear un commando, retornando `CommandErr` en caso de error.
readCommand :: String -> Either CommandErr Command
readCommand = first (const (undefined :: CommandErr)) <$> parse readCommand' "."

-- | logica del REPL. 
repl' :: StateT (SymbolTable Identificador Valor) (ExceptT Error IO) b
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