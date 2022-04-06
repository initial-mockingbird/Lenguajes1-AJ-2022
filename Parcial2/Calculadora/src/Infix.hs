module Infix where

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving (Show, Eq)

foldTree :: (a -> b -> b -> b) -> (c -> b) -> BinTree a c -> b 
foldTree _ g (Leaf x) = g x
foldTree f x (Node a l r) =  f a lVal rVal
    where 
        lVal = foldTree f x l
        rVal = foldTree f x r

newtype LeafNode = LN {getLF :: Int} deriving (Show, Eq)

data Operator
    = Plus
    | Minus
    | Mult
    | Div 
    deriving (Eq, Show)

data AssocDir = LeftAssoc | RightAssoc 


getAssoc :: Operator -> AssocDir
getAssoc _ = LeftAssoc

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
regenerateAST (AST t) = snd $ foldTree f (\ln -> (maxPrecLevel + 1, prettyLeaf ln, Nothing )) t 
    where

        snd (_,s,_) = s

        f :: Operator -> (Int,String, Maybe Operator) -> (Int,String, Maybe Operator) -> (Int,String, Maybe Operator)
        f nd (precL,l, ml) (precR,r, mr) = (precNd,lStr ++ ' ':prettyOperator nd ++ ' ':rStr , Just nd)
            where
                precNd = getPrec nd 
                lStr = parenthesizePrec precNd precL nd ml LeftAssoc l
                rStr = parenthesizePrec precNd precR nd mr RightAssoc r 

                --lStr = parenthesizeAssoc (Just nd) ml LeftAssoc  lStr' 
                --rStr = parenthesizeAssoc (Just nd) mr RightAssoc rStr' 

        parenthesizePrec :: Int -> Int -> Operator -> Maybe Operator -> AssocDir -> (String -> String)
        parenthesizePrec father child f mc assoc 
            | father > child  = \s -> "(" ++ s ++ ")"
            | father == child = parenthesizeAssoc (Just f) (Just f) assoc
            | otherwise                      = id 

        parenthesizeAssoc :: Maybe Operator -> Maybe Operator -> (AssocDir -> String -> String)
        parenthesizeAssoc Nothing _  =  const id
        parenthesizeAssoc _ Nothing  = const id
        parenthesizeAssoc (Just op) (Just op')
            | op == op'  = case getAssoc op of 
                LeftAssoc  -> g
                RightAssoc -> f
            | otherwise  = const id 
                where
                    f LeftAssoc  s = "(" ++ s ++ ")"
                    f RightAssoc s = s

                    g RightAssoc  s = "(" ++ s ++ ")"
                    g LeftAssoc   s = s




operate :: AST -> Int
operate (AST t) = foldTree f getLF t 
    where
        f Plus  = (+)
        f Minus = (-)
        f Mult  = (*)
        f Div   = div 
