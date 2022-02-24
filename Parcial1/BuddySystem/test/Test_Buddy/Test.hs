{-# LANGUAGE RecordWildCards #-}

module Test_Buddy.Test where

import Buddy_System.Buddy_Internal.Buddy
import Test.Hspec
import Data.Either
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Test.QuickCheck.Gen
import Debug.Trace

newtype BlockNoChildren = BNC {getBlock       :: Tree MemoryNode} deriving Show

newtype SplittableBlock = SB  { getBlockSize  :: (Tree MemoryNode,Int) } deriving Show

newtype RandomBlock     = RB  {getRandomBlock :: Tree MemoryNode } deriving Show

instance Arbitrary BlockNoChildren where
    arbitrary = do
        size <- abs <$> arbitrary
        return $ BNC {getBlock=Node MN {value=size, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf}

instance Arbitrary SplittableBlock where
    arbitrary  = do
        blockSize <- chooseInt (2,10)
        askedSize <- chooseInt (1,blockSize - 1)
        return $ SB (Node MN {value=blockSize, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf, askedSize)

instance Arbitrary  RandomBlock where
    arbitrary =  RB <$> go n1
        where
            n1 = Node MN {value=2, leftValues=Map.fromList [(1,1)], rightValues=Map.fromList [(1,1)]} n2 n3
            n2 = Node MN {value=1, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf
            n3 = Node MN {value=1, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf

            go n@(Node MN {..} l r) = do
                dir <- frequency  [(4,return $ Just L), (2, return $ Just R), (1, return Nothing)]
                let newNode = Node MN {value=value, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf 
                case dir of
                  Nothing -> return n
                  Just L  -> go $ Node MN {value=value+1, leftValues=getNodeValues n, rightValues=Map.fromList [(value,1)]} n newNode
                  Just R  -> go $ Node MN {value=value+1, leftValues=Map.fromList [(value,1)], rightValues=getNodeValues n} newNode n
            go _ = undefined 
    
                    



---------------
-- malloc'
---------------

isLeftSkewed :: Tree a -> Bool
isLeftSkewed Leaf = True
isLeftSkewed (Node _ l (Node _ Leaf Leaf)) = isLeftSkewed l
isLeftSkewed _               = False



mallocSpec' :: Spec
mallocSpec' = describe "malloc' internals" $ do
    prop "Returns an error on empty trees"
        (\n -> isLeft (runWriterT $ malloc' n Leaf))

    prop "When given a block whose size is equal to the asked size, malloc' fills it"
        (\BNC {getBlock=nm@(Node MN{..} _ _) }-> case fmap fromReversedEndo <$> runWriterT (malloc' (value,value) nm) of Right (Leaf,[]) -> True; _ -> False )

    prop "When given a block whose size is GREATER than the asked size, malloc' partitions the block (in a left skewed manner!)"
        (\SB {getBlockSize=(sb@(Node nm _ _),size)} ->
            case fmap fromReversedEndo <$> runWriterT (malloc' (size,size) sb) of
                Right (memTree,crumbs) -> let
                    walkHasRightLength = length crumbs == value nm - size
                    skewedForm         = isLeftSkewed memTree
                    correctCrumbs      = all (==L) crumbs
                    in walkHasRightLength && skewedForm && correctCrumbs
                _                      -> False
            )
    prop "When given a block whose size is SMALLER than the asked size, malloc' yields an error"
        (\BNC {getBlock=nm@(Node MN{..} _ _) }-> case runWriterT $ malloc' (value+1,value+1) nm of Left _ -> True; _ -> False )
    
    prop "On already fragmented memory, if able, malloc' takes a left"
        (\RB {..} -> case fmap fromReversedEndo <$> runWriterT (malloc' (1, 1) getRandomBlock) of Right (_,L:_) -> True; _ -> False)
    
    



