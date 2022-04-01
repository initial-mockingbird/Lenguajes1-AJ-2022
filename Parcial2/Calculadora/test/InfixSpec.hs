{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module InfixSpec where


import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Control.Monad 
import Infix

-- 5 + 2 * 3
a1 :: AST 
a1 = AST $ Node Plus (Leaf $ LN 5) (Node Mult (Leaf $ LN 2) (Leaf $ LN 3))

-- (5 + 2) * 3
a2 :: AST 
a2 = AST $ Node Mult (Node Plus (Leaf $ LN 5) (Leaf $ LN 2)) (Leaf $ LN 3)


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
        let toList = foldTree (\a l r -> Left a : l ++ r) (\x -> [Right x])
        in 
            [Right $ LN 5, Left Plus, Right $ LN 2, Left Mult, Right $ LN 3 ] == toList a1
            -- &&
            -- [Right $ LN 5, Left Plus, Right $ LN 2, Left Mult, Right $ LN 3 ] == toList a2

        
infixSpec :: IO ()
infixSpec = forM hspec  [precSpec, foldSpec]
