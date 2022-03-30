module Infix where

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b)

foldTree :: (a -> b -> b -> b) -> (c -> b) -> BinTree a c -> b 
foldTree _ g (Leaf x) = g x
foldTree f x (Node a l r) =  f a lVal rVal
    where 
        lVal = foldTree f x l
        rVal = foldTree f x r

newtype LeafNode = LN {getLF :: Int} deriving Show

data Operator
    = Plus
    | Minus
    | Mult
    | Div 
    deriving (Eq, Show)

newtype AST = AST {getAST :: BinTree Operator LeafNode}

getPrec :: Operator -> Int
getPrec nd 
    | nd `elem` prec1 = 1
    | nd `elem` prec2 = 2
    | otherwise       = error ""
    where 
        prec1 = [Plus,Minus]
        prec2 = [Mult,Div]

maxPrecLevel :: Int
maxPrecLevel = 2

prettyOperator :: Operator -> String
prettyOperator Plus  = "+"
prettyOperator Minus = "-"
prettyOperator Mult  = "*"
prettyOperator Div   = "/"

prettyLeaf :: LeafNode -> String
prettyLeaf (LN n)   = show n


regenerateAST :: AST -> String
regenerateAST (AST t) = snd $ foldTree f (\ln -> (maxPrecLevel + 1, prettyLeaf ln)) t 
    where
        f :: Operator -> (Int,String) -> (Int,String) -> (Int,String)
        f nd (precL,l) (precR,r) = (precNd,lStr ++ ' ':prettyOperator nd ++ ' ':rStr )
            where
                precNd = getPrec nd 
                lStr = parenthesize precNd precL l
                rStr = parenthesize precNd precR r 

        parenthesize :: Int -> Int -> (String -> String)
        parenthesize father child 
            | father > child = \s -> "(" ++ s ++ ")"
            | otherwise                      = id 


operate :: AST -> Int
operate (AST t) = foldTree f getLF t 
    where
        f Plus  = (+)
        f Minus = (-)
        f Mult  = (*)
        f Div   = div 
