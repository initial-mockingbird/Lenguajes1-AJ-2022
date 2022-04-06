module Types where

data Atomico = Atomico
    { nombreAtomico  :: String 
    , representacion :: Int
    , alineacion     :: Int
    }

data Struct = Struct 
    { nombreStruct :: String 
    , tipos        :: [Tipo]
    }

data Arreglo = Arreglo  
    { nombreArreglo :: String
    , tipoArreglo   :: Tipo
    , size          :: Int
    }

data Tipo = At Atomico | St Struct | Arr Arreglo

getNombre :: Tipo -> String
getNombre (At a)  = nombreAtomico a
getNombre (St s)  = nombreStruct  s
getNombre (Arr a) = nombreArreglo a

-- | Medido en bytes
wordSize :: Int
wordSize = 4

byteRatio :: Int
byteRatio = 3

{-
drawBox :: Int -> Tipo -> String
drawBox maxSize tipo = leadingSpace ++ header ++ "\n" ++ drawBox' 0 maxSize tipo
    where
        halfSize = maxSize `div` 2 
        leadingSpace = repeat ' ' $ halfSize * byteRatio - div (length header) 2
        header   = show wordSize ++ " bytes"

drawBox' :: Int -> Int -> Tipo -> String
drawBox currentBlock maxSize (A a) = 
-}




