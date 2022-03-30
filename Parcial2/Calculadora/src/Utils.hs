module Utils where 

import Infix
import Parser
import Control.Monad 


data Mode          = Pre  | Post deriving Show 
data Functionality = Eval | Print deriving Show


translate :: Mode -> [Token] -> [Either LeafNode AST] -> Either Error AST 
translate md (Terminal ln:tks) lns   = translate md tks (Left ln:lns) 
translate md (TkOp op:tks) (l:r:lns) = translate md tks (Right (AST newT) : lns)
    where
        f (Left ln) = Leaf ln
        f (Right (AST t)) = t 

        newT = builder md op (f l) (f r)

translate md [] [Right t] = pure t 
translate _ _ _           = Left SyntaxError

builder :: Mode -> a -> BinTree a b -> BinTree a b -> BinTree a b
builder Pre  op = Node op 
builder Post op = flip (Node op)


eval :: Mode -> String -> Either Error Int
eval mode = preprocess mode >=> pure . operate 

toString :: Mode -> String -> Either Error String
toString mode = preprocess mode >=> pure . regenerateAST 

preprocess :: Mode -> String -> Either Error AST
preprocess mode = parseStr >=> f . g 
    where 
        f tks = translate mode tks [] 
        g = case mode of Post -> id; Pre -> reverse;

mkAction :: Functionality -> Mode -> String -> Either Error String
mkAction Print = toString 
mkAction Eval  = (.) (fmap show) . eval 
