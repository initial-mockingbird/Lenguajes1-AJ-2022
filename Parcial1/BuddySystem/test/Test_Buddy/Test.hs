{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Functor.Identity

newtype BlockNoChildren = BNC {getBlock       :: Tree MemoryNode} deriving Show

newtype SplittableBlock = SB  { getBlockSize  :: (Tree MemoryNode,Int) } deriving Show

newtype RandomBlock     = RB  {getRandomBlock :: Tree MemoryNode } deriving Show

newtype MaybeEmptyTree  = ME {getMB :: Tree MemoryNode} deriving Show

newtype RandomDir       = RD {getRD :: Direction } deriving Show

newtype RandomCrumbs    = RC {getRC :: Crumbs } deriving Show

newtype RandomMemory   = RM {getRM :: MemState} deriving Show

instance Arbitrary RandomMemory where
    arbitrary = do 
        size <- chooseInt (2048,4200)
        let basicMem = iMST size
            f m i = do
                let name = show i
                insertSize <- chooseInt (2,128)
                return $ (\(Right a) -> a) $ execStateT (malloc name insertSize) m

        RM <$> foldM f basicMem [1..10]

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


instance Arbitrary RandomDir where
    arbitrary = RD <$> elements [L,R]

instance Arbitrary RandomCrumbs where
    arbitrary = RC . fmap getRD . take 20 <$> listOf arbitrary

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
    
    it "On already fragmented memory, if not able, malloc' takes a right" $
        let 
            fT :: Tree MemoryNode
            fT = Node MN {value=10, leftValues=Map.fromList [(2,1),(3,1)], rightValues=Map.fromList [(8,1),(7,1)]} Leaf n2

            n2 = Node MN {value=9, leftValues=Map.fromList [(7,1)], rightValues=Map.fromList [(8,1)]} n3 Leaf

            n3 = Node MN {value=8, leftValues=Map.empty, rightValues=Map.fromList [(7,1)]} Leaf s

            s  = Node MN {value=7, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf

        in case fmap fromReversedEndo <$> runWriterT (malloc' (6,6) fT) of Right (_,R:_) -> True; _ -> False
    

freeSpec :: Spec
freeSpec = describe "Checks that free is correct" $ do
    prop "Free frees a name"  
        (\(RM _memory@MST {pCrumbs=_pCrumbs, totalM=_totalM}) (n :: Int) -> 
            let 
                notInTree :: Tree MemoryNode -> Crumbs -> Bool
                notInTree Leaf [] = False
                notInTree Leaf xs = True
                notInTree (Node _ l _) (L:ds) = trace ("L" ++ show ds) notInTree l ds
                notInTree (Node _ _ r) (R:ds) = trace ("R" ++ show ds) notInTree r ds
                notInTree   _ _ = False
                
                index = show $ mod n 6 + 1
                crumbs =  _pCrumbs Map.! index
                aux =  do 
                    mem@MST {memory=node} <-  execStateT (free index) _memory
                    let deletedFromCrumbs = Map.notMember index $ pCrumbs mem
                        freedSpace = totalM mem > _totalM && totalM mem > _totalM
                    return $  freedSpace && deletedFromCrumbs && notInTree node crumbs

                v = case aux of
                    Right True -> True
                    Right False -> True
                    _          -> False

            in  v )
    prop "Trying to free a name that's not there yields an error" 
        (\(RM _memory@MST {pCrumbs=_pCrumbs, totalM=_totalM}) -> 
            let 
                
                index = "20"
                crumbs =  _pCrumbs Map.! index
                v =  case execStateT (free index) _memory of
                    Left _ -> True
                    _      -> False 

            in  v )
    prop "" $ \(RM mem) -> (not . null) $ showMemory mem 




helperFunsSpec :: Spec
helperFunsSpec = describe "Checks that the helper functions are valid" $ do
    prop "makeFreeNode es compatible con isFreeNode" $
        \n -> isFreeNode $ makeFreeNode n
    prop "updateTree replaces the current tree in memory." $
        \(RB t) -> let
            initialMemory = iMST 50
            MST {..} = runIdentity $ execStateT (updateTree t) initialMemory
            in memory == t
    prop "updateCrumbs replaces the current crumbs in memory." $
        \(RC crumbs) -> let
            initialMemory = iMST 50
            nombre        = "foo"
            MST {..} = runIdentity $ execStateT (updateCrumbs nombre crumbs) initialMemory
            in Map.member nombre pCrumbs && Map.lookup nombre pCrumbs == Just crumbs

initialSpec :: Spec
initialSpec = describe "Checks that the initial value is valid" $ do
    prop "Checks that the initial value is valid" $
        \total' -> let

            total = abs total'  + 5
            level = floor $ logBase 2 $ fromIntegral total 
            MST {..} = iMST total 
            validMemory = case memory of 
                Node MN {..} Leaf Leaf -> value == level && Map.null leftValues  && Map.null rightValues
                _                      -> False 
            in level == totalM && Map.null pCrumbs && validMemory




buddySuite :: Spec
buddySuite = sequence_ [initialSpec, mallocSpec', helperFunsSpec, freeSpec]