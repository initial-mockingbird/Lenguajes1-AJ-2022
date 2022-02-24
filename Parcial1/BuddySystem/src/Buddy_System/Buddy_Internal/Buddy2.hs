{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Buddy_System.Buddy_Internal.Buddy2 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 
import Data.Bifunctor
import Control.Applicative
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Except

---------------------------------
-- Types and Data
---------------------------------

-- | Binary Tree
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

-- | Either Left or Right for Binary Trees.
data Direction = L | R deriving (Eq,Show)

-- | Nodes of the Tree.
data MemoryNode = MN 
    { value       :: Int    -- ^ Total Memory of the Block.
    , leftValues  :: ValRep 
    , rightValues :: ValRep  
    } deriving Show


type Nombre = String 

-- | Leaving a Trail of crumbs to locate names
type Crumbs = [Direction]  

-- | A Map associating: Free Block sizes to the number of times it occurs.
type ValRep = Map Int Int 

-- | Current state of the memory.
data MemState = MST 
    { memory  :: Tree MemoryNode   -- ^ Actual Memory!
    , pCrumbs :: Map Nombre Crumbs -- ^ A way to locate names. 
    }

-- | Holds all the possible errors relating to handling memory.
data Errors = InsufficientMemoryE  | FragmentationE | AlreadyExecutingE | CHECKYOURCODE deriving Show

 


---------------------------------
-- Accesibility
---------------------------------

-- | replaces the current memory tree.
updateTree :: Monad m => Tree MemoryNode -> StateT MemState m ()
updateTree t = get >>= \mst -> put mst{memory=t}
    
-- | Update the associating
updateCrumbs :: Monad m => Nombre -> Crumbs -> StateT MemState m ()
updateCrumbs nombre crumbs = get >>= \mst@MST{..} -> put mst{pCrumbs = Map.insert nombre crumbs pCrumbs}
    
-- | Just tell me how much free space do I have (since the tree is bounded by log(space), repeatedly
-- fusing this dicts shouldn't be much of a bottleneck).
getNodeValues :: Tree MemoryNode -> ValRep
getNodeValues Leaf              = Map.empty 
getNodeValues (Node MN{..} _ _) = Map.unionWith (+) leftValues rightValues

---------------------------------
-- Checkers?
---------------------------------

-- | Checks whether a name is already in the executing list.
alreadyInCheck :: MonadError Errors m  => Nombre -> StateT MemState m ()
alreadyInCheck nombre = do 
    b <- Map.member nombre . pCrumbs <$> get  
    when b (throwError AlreadyExecutingE)

---------------------------------
-- Functions
---------------------------------

-- Should I refactor this and drop the first Tree argument in order to have: ReaderT + WriterT 
-- or alternatively StateT?

-- | Allocates `size` in the Memory tree, appending the Crumbs in order to de-allocate it in the future.
malloc' :: MonadError Errors m 
    => Int             -- ^ Depth! (not size, it's cheapear to substract 1 in each iteration rather than divide by 2.) 
    -> Tree MemoryNode -- ^ The memory tree.
    -> WriterT Crumbs m  (Tree MemoryNode)
malloc' size (Node mn@MN {..} Leaf Leaf) 
    -- We have found the value! erase the node by returning a Leaf
    | size == value = return Leaf
    -- If we havent found the value, and we just arrived at a block that has sufficient memory, 
    -- then partition it baby.
    | size < value  = do 
        let newNode = Node MN {value=nextLevel, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf
        left  <- malloc' size newNode 
        tell [L] 
        return $ Node mn{leftValues=getNodeValues left, rightValues=Map.singleton nextLevel 1} left newNode
    -- We just arrived at a block, whose value is less than the desire size. 
    -- throw so we must throw a Full Memory error!.
    | otherwise  = throwError InsufficientMemoryE
    where
        nextLevel = value-1 
malloc' size (Node mn@MN{..} left right) = case bimap f f (leftValues,rightValues) of 
    -- notice that by pattern matching like this, the search over the right might not be executed!
    -- nevertheless this creates SUPER skewed (to the left) trees, Somebody should fix that.....
    -- (cuz I ain't doing an AVL tree >:C)
    (Just s,_)         -> do
        left' <- malloc' s left
        tell [L]
        return (Node mn{leftValues=getNodeValues left'} left' right)
    (_,Just s)         -> do 
        right' <- malloc' s right
        tell [R]
        return (Node mn{rightValues=getNodeValues right'} left right')
    -- This can only happen at the very first iteration! (since we are feeding the exact size in the two cases above)
    -- so we can know if it's a fragmentation error (trying to allocate something that's less than the total memory, but failing due to (duh) fragmentation)
    -- or an insufficient memory error (program is just TOO big).
    (Nothing, Nothing) -> if size <= value then throwError FragmentationE else throwError InsufficientMemoryE
    where 
        -- search increasingly from size up to the maximum block value in `xs`
        -- return the first MINIMUM size if found, return nothing if no block can be allocated.
        f xs = foldr (\n acc -> acc <|> (n <$ Map.lookup n xs) ) Nothing [size..value]
-- Pattern _ Leaf never happens since this insertion is biased towards left,
malloc' _ Leaf = throwError CHECKYOURCODE

-- | :) please be ok.
malloc :: MonadError Errors m => Nombre -> Int -> StateT MemState m ()
malloc nombre size = do
    alreadyInCheck nombre
    let size' = ceiling $ logBase 2 $ fromIntegral size
    memTree <- memory <$> get
    (memTree',crumbs) <- runWriterT $ malloc' size' memTree
    updateTree memTree'
    updateCrumbs nombre crumbs
    return ()

