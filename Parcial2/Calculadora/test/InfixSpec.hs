{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module InfixSpec where


import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Control.Monad 
import Infix
import Data.Foldable

-- 5 + 2 * 3
a1 :: AST 
a1 = AST $ Node Plus (Leaf $ LN 5) (Node Mult (Leaf $ LN 2) (Leaf $ LN 3))

-- (5 + 2) * 3
a2 :: AST 
a2 = AST $ Node Mult (Node Plus (Leaf $ LN 5) (Leaf $ LN 2)) (Leaf $ LN 3)

-- 1 - (2 - 3)
a3 :: AST 
a3 = AST $ Node Minus (Leaf $ LN 1) (Node Minus (Leaf $ LN 2) (Leaf $ LN 3))

an :: Operator  -> AST 
an op = AST $ Node op (Leaf $ LN 1) (Node op (Leaf $ LN 3) (Leaf $ LN 2))

precSpec :: Spec
precSpec = describe "Tests precedence related stuff" $ do
    it "Plus and Minus have the same precedence" $
        getPrec Plus == getPrec Minus
    it "Multiply and Integer division have the same precedence" $
        getPrec Mult == getPrec Div
    it "Precedence of Plus is strictly less than the precedence of Mult" $
        getPrec Plus < getPrec Mult 

foldSpec :: Spec 
foldSpec = describe "Checks whether the fold behaves correctly." $ do 
    it "foldTree is able to catamorph into a list: " $
        let toList = foldTree (\a l r ->  l ++ Left a : r) (\x -> [Right x])
        in 
            [Right $ LN 5, Left Plus, Right $ LN 2, Left Mult, Right $ LN 3 ] == toList (getAST a1)
            &&
            [Right $ LN 5, Left Plus, Right $ LN 2, Left Mult, Right $ LN 3 ] == toList (getAST a2)

regenerateASTSpec :: Spec 
regenerateASTSpec = describe "Checks if the AST is getting correctly regenerated." $ do
    it "a1 = 5 + 2 * 3"   $ regenerateAST a1 == "5 + 2 * 3"
    it "a2 = (5 + 2) * 3" $ regenerateAST a2 == "(5 + 2) * 3"
    it "a3 = 1 - (2 - 3)" $ regenerateAST a3 == "1 - (2 - 3)"
    it "an + = 1 + (3 + 2) " $ regenerateAST (an Plus) == "1 + (3 + 2)"
    it "an * = 1 * (3 * 2) " $ regenerateAST (an Mult) == "1 * (3 * 2)"
    it "an / = 1 / (3 / 2) " $ regenerateAST (an Div) == "1 / (3 / 2)"

operateSpec :: Spec
operateSpec = describe "Checks if the AST is getting correctly evaluated." $ do 
    it "a1 = 5 + 2 * 3   = 11 "  $ operate a1 == 11
    it "a2 = (5 + 2) * 3 = 21 "  $ operate a2 == 21
    it "a3 = 1 - (2 - 3) =  2 "  $ operate a3 == 2
    it "an + = 1 + (2 + 3) = 6 " $ operate (an Plus) == 6
    it "an * = 1 * (2 * 3) = 6 " $ operate (an Mult) == 6
    it "an / = 1 / (3 / 2) = 1 " $ operate (an Div) == 1
        
infixSpec :: IO ()
infixSpec = traverse_ hspec  [precSpec, foldSpec, regenerateASTSpec, operateSpec]
