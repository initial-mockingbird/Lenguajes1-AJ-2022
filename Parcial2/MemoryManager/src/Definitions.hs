{-# LANGUAGE FlexibleContexts #-}

module Definitions where


import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
    ( MVar, newMVar, tryReadMVar, tryTakeMVar ) 
import Control.Monad.State.Strict
    ( MonadState(put, get), StateT, MonadIO(..), (>=>), void )
import Control.Monad.Except
    ( MonadError(throwError), MonadIO(..), (>=>), void )
import Control.Monad.Morph ()
import Control.Monad ( (>=>), void ) 
import Data.Functor ( void ) 

-- Utilizaremos un razonamiento analogo a las lapidas
-- introduciendo un nivel adicional de indireccion.

-------------------------------
-- Data types
-------------------------------

-- | La tabla de simbolos asocia a cada identificador a un `MVar` que "apunta" a un valor.
--en este caso, la "lapida" seria el wrapper `MVar`.
newtype SymbolTable a b = ST {getST :: Map a (MVar b)}

-- | Todos los posibles errores que puede soltar nuestro manejador.
data Error = SymbolNotDefined | AlreadyFreed | AlreadyDefined -- este ultimo no se usa! esperandito respuesta del profe.

-- | Resulta que buscar el valor de un apuntador y liberarlo son **casi** lo mismo, y podemos reutilizar bastante codigo
-- si pudiesemos parametrizar la funcion de dereferencia.
data DereferenceMode = ReadMode | TakeMode 


-- | Resulta que crear un alias y pedir memoria son **casi** lo mismo... Asi que podemos reutilizar codigo si podemos
-- parametrizar la funcion qu `set`ea la memoria.
data Value a = Atomic a | Pointer (MVar a)

-------------------------------
-- Initial State
-------------------------------

-- | Tabla de simbolo inicial.
initialST :: Ord a => SymbolTable a b
initialST = ST Map.empty

-------------------------------
-- Functions
-------------------------------

-- | Dado un valor, retorna la "lapida" asociada a el, o suelta un error: `SymbolNotDefined` en caso de que
-- el valor no exista en memoria.
lookupSymbol :: (Ord a,Monad m, MonadIO m, MonadError Error m) => a -> StateT (SymbolTable a b) m (MVar b)
lookupSymbol var = get >>= morphedLookup var . getST 
    where
        hoistMaybe (Just a)  = pure a
        hoistMaybe Nothing   = throwError SymbolNotDefined 
        morphedLookup var    = hoistMaybe . Map.lookup var 

-- | Dado un modo de deferenciacion, y una "lapida", obtenemos el valor al que esta apuntando la 
-- lapida, destruyendo el puntero si el modo es `TakeMode`, o dejandolo intacto si es `ReadMode`.
-- suelta una excepcion en caso de que ya se haya liberado la memoria previamente.
dereference :: (Monad m, MonadIO m, MonadError Error m) => DereferenceMode -> MVar a -> m a
dereference mode mvA =  liftIO (f mvA) >>= hoistMaybe
    where
        hoistMaybe (Just a) = pure a
        hoistMaybe Nothing  = throwError AlreadyFreed

        f = case mode of ReadMode -> tryReadMVar; TakeMode -> tryTakeMVar

-- | Dado un identificador y un valor, asigna el valor al identidicador en la tabla de simbolos.
set :: (Ord a,Monad m, MonadIO m) => a -> Value b -> StateT (SymbolTable a b) m ()
set var val = do
    table <- getST <$> get
    mvVal <- case val of Atomic val' -> liftIO (newMVar val'); Pointer val' -> pure val' 
    put . ST $ Map.insert var mvVal table   -- mvar goes brrrrr, "crear" multiples copias del mismo mvar es lo mismo que tener
                                            -- una unica, puesto que todas apuntan a la misma cajita.


-- | Dado un identificador, trata de buscar el valor asociado a el. Suelta `AlreadyFreed` o `SymbolNotDefined`
-- en caso de error.
querySymbol :: (Ord a,Monad m, MonadIO m, MonadError Error m) => a -> StateT (SymbolTable a b) m b
querySymbol = lookupSymbol >=> dereference ReadMode

-- | Dado un identificador, trata de liberar el espacio al que este apunta, soltando: `AlreadyFreed` o `SymbolNotDefined`
-- en caso de error.
free :: (Ord a,Monad m, MonadIO m, MonadError Error m) => a -> StateT (SymbolTable a b) m ()
free = lookupSymbol >=> void . dereference TakeMode

-- | Dado un identificador y un valor, alloca memoria, y asigna el valor.
malloc :: (Ord a,Monad m, MonadIO m) => a -> b -> StateT (SymbolTable a b) m ()
malloc var = set var . Atomic 

-- | Dado dos identificadores, crea un alias entre ambos, soltando `AlreadyFreed` o `SymbolNotDefined`
-- en caso de error.
assign :: (Ord a,Monad m, MonadIO m, MonadError Error m) => a -> a -> StateT (SymbolTable a b) m ()
assign to from = lookupSymbol from >>= set to . Pointer
