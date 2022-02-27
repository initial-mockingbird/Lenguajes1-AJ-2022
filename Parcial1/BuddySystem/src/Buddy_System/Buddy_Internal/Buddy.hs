{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Buddy_System.Buddy_Internal.Buddy where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 
import Data.Bifunctor
import Control.Applicative
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Tree (drawTree)
import qualified Data.Tree as Tree 
import Data.Monoid

---------------------------------
-- Types and Data
---------------------------------

-- | Binary Tree
data Tree a = Leaf | Node a (Tree a) (Tree a)  deriving (Eq)

-- | Either Left or Right for Binary Trees.
data Direction = L | R deriving (Eq,Show)

-- | Nodes of the Tree.
data MemoryNode = MN 
    { value       :: Int    -- ^ Total Memory of the Block.
    , leftValues  :: ValRep 
    , rightValues :: ValRep  
    } deriving (Eq,Show)


type Nombre = String 

-- | Leaving a Trail of crumbs to locate names
type Crumbs = [Direction]  

-- | A Map associating: Free Block sizes to the number of times it occurs.
type ValRep = Map Int Int 

-- | Current state of the memory.
data MemState = MST 
    { memory  :: Tree MemoryNode   -- ^ Actual Memory!
    , pCrumbs :: Map Nombre Crumbs -- ^ A way to locate names. 
    , totalM  :: Int               -- ^ Total memory blocks.
    } deriving Show

-- | Holds all the possible errors relating to handling memory.
data Errors = InsufficientMemoryE  | FragmentationE | AlreadyExecutingE | NotInMemoryE  deriving Show

---------------------------------
-- Instances
---------------------------------

-- Pal debuggeo intenso
instance (Show a) => Show (Tree a) where
    show t = drawTree t' 
        where
            trans :: Show a => Tree a -> Tree.Tree String
            trans Leaf               = Tree.Node "Leaf" []
            trans (Node v Leaf Leaf) = Tree.Node (show v) [Tree.Node "Leaf" [], Tree.Node "Leaf" []]
            trans (Node v l Leaf)    = Tree.Node (show v) [trans l, Tree.Node "Leaf" []]
            trans (Node v Leaf r)    = Tree.Node (show v) [Tree.Node "Leaf" [], trans r]
            trans (Node v l r)       = Tree.Node (show v) [trans l,trans r]

            t' = trans t

-- Creo q no se usa....
instance Functor Tree where
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
    fmap _ Leaf         = Leaf


---------------------------------
-- Initials
---------------------------------

iMST :: Int -> MemState
iMST total = MST {memory=Node MN {value=total', leftValues=Map.empty , rightValues=Map.empty} Leaf Leaf, pCrumbs=Map.empty, totalM=total'}
    where
        total' = max 1 $ floor $ logBase 2 $ fromIntegral total
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

-- | Since I didn't wanted to create a newtype for lists so it's monoid instante was
-- a flipped version of the original, we just reverse the endo for lists jujuju.
fromReversedEndo :: Endo [a] -> [a]
fromReversedEndo = reverse . (`appEndo`  [])

-- | Checks whether a node is Free or not.
isFreeNode :: Tree MemoryNode -> Bool
isFreeNode (Node _ Leaf Leaf) = True
isFreeNode _                  = False

makeFreeNode :: Int -> Tree MemoryNode 
makeFreeNode val = Node MN {value=val, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf


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
    => (Int,Int)             -- ^ Depth! (not size, it's cheapear to substract 1 in each iteration rather than divide by 2.) 
    -> Tree MemoryNode -- ^ The memory tree.
    -> WriterT (Endo Crumbs) m  (Tree MemoryNode)
malloc' p@(actualBlockSize,inflatedSize) (Node mn@MN {..} Leaf Leaf) 
    -- We have found the value! erase the node by returning a Leaf
    | actualBlockSize == value = return Leaf
    -- If we havent found the value, and we just arrived at a block that has sufficient memory, 
    -- then partition it baby.
    | actualBlockSize < value  = do 
        let newNode = Node MN {value=nextLevel, leftValues=Map.empty, rightValues=Map.empty} Leaf Leaf
        left  <- malloc' p newNode 
        tell $ Endo ([L]  <>)
        return $ Node mn{leftValues=getNodeValues left, rightValues=Map.singleton nextLevel 1} left newNode
    -- We just arrived at a block, whose value is less than the desire size. 
    -- throw so we must throw a Full Memory error!.
    | otherwise  = throwError InsufficientMemoryE
    where
        nextLevel = value-1 
malloc' (actualBlockSize,inflatedSize) (Node mn@MN{..} left right) = case bimap f f (leftValues,rightValues) of 
    -- notice that by pattern matching like this, the search over the right might not be executed!
    -- nevertheless this creates SUPER skewed (to the left) trees, Somebody should fix that.....
    -- (cuz I ain't doing an AVL tree >:C)
    (Just s,_)         -> do
        left' <- malloc' (actualBlockSize,s) left
        tell $ Endo ([L] <>)
        return (Node mn{leftValues=getNodeValues left'} left' right)
    (_,Just s)         -> do 
        right' <- malloc' (actualBlockSize,s) right
        tell $ Endo ([R] <>)
        return (Node mn{rightValues=getNodeValues right'} left right')
    -- This can only happen at the very first iteration! (since we are feeding the exact size in the two cases above)
    -- so we can know if it's a fragmentation error (trying to allocate something that's less than the total memory, but failing due to (duh) fragmentation)
    -- or an insufficient memory error (program is just TOO big).
    (Nothing, Nothing) -> if actualBlockSize <= value then throwError FragmentationE else throwError InsufficientMemoryE
    where 
        -- search increasingly from size up to the maximum block value in `xs`
        -- return the first MINIMUM size if found, return nothing if no block can be allocated.
        f xs = foldr (\n acc -> acc <|> (n <$ Map.lookup n xs) ) Nothing $ reverse [inflatedSize .. value]
malloc' _ Leaf = throwError InsufficientMemoryE

-- | :) please be ok.
malloc :: MonadError Errors m => Nombre -> Int -> StateT MemState m ()
malloc nombre size = do
    alreadyInCheck nombre
    let size' = max 1 $ ceiling $ logBase 2 $ fromIntegral size
    memTree <- memory <$> get
    (memTree',crumbs) <- fmap fromReversedEndo <$> runWriterT (malloc' (size',size') memTree)
    updateTree memTree'
    updateCrumbs nombre crumbs

-- | Frees a name from memory
free :: MonadError Errors m => Nombre -> StateT MemState m ()
free nombre = do
    -- get the crumbs associated with the program
    crumbs' <-  Map.lookup nombre . pCrumbs <$> get
    maxMem  <- totalM <$> get 
    -- if free yields an empty tree, create a new one with maximum memory.
    let free'' n t = case free' n t of Leaf -> makeFreeNode maxMem; t -> t
    case  crumbs' of
        Nothing     -> throwError NotInMemoryE -- name not found, yield an error
        Just crumbs -> do  -- name found, update tree
            t <- get  
            (updateTree . free'' crumbs . memory) t
            modify (\t@MST{..} -> t{pCrumbs= Map.delete nombre pCrumbs})

-- | Free from the tree, just traverse it and then unify siblings.
free' :: Crumbs -> Tree MemoryNode -> Tree MemoryNode
free' []  _        = Leaf 
free' [L] (Node v@MN{..} _ r) 
    | isFreeNode r = makeFreeNode value
    | otherwise    = Node v{leftValues=Map.fromList [(value-1,1)]} (makeFreeNode (value-1)) r
free' [R] (Node v@MN{..} l _) 
    | isFreeNode l = makeFreeNode value
    | otherwise    = Node v{rightValues= Map.fromList [(value-1,1)]} l (makeFreeNode (value-1)) 
free' (L:dirs) (Node v l r) 
    | isFreeNode l' && isFreeNode r = makeFreeNode (value v)
    | isFreeNode l'                 = Node v{leftValues=Map.fromList [(value v-1,1)]} l' r
    | otherwise                     = Node v{leftValues=getNodeValues l'} l' r
    where
        l' = free' dirs l
free' (R:dirs) (Node v l r) 
    | isFreeNode r' && isFreeNode l = makeFreeNode (value v)
    | isFreeNode r'                 = Node v{rightValues=Map.fromList [(value v-1,1)]} l r'
    | otherwise                     = Node v{rightValues=getNodeValues r'} l r'
    where
        r' = free' dirs r
free' _ _ = undefined 


-- | A human readable representation of the memory.
showMemory :: MemState -> String
showMemory MST {..} = case memory of
    Leaf -> let nombre = fst $ head $ Map.toList pCrumbs in  "| Block size: " ++ show (2^totalM) ++ ", Status: Occupied,    Name: " ++ nombre ++ "\n"
    _    -> toString modified
    where
        coupled :: Tree (MemoryNode, Maybe a)
        coupled = fmap (,Nothing) memory

        replace :: Crumbs -> Nombre -> Tree (MemoryNode, Maybe Nombre) -> Tree (MemoryNode, Maybe Nombre)
        replace _  _ Leaf = Leaf
        replace [] _ t    = t
        replace [L]      nombre (Node a@(a',_) l r) = Node a   (Node (a'{value=value a'-1}, Just nombre) Leaf Leaf) r
        replace [R]      nombre (Node a@(a',_) l r) = Node a l (Node (a'{value=value a'-1}, Just nombre) Leaf Leaf)
        replace (L:dirs) nombre (Node a l r) = Node a (replace dirs nombre l) r
        replace (R:dirs) nombre (Node a l r) = Node a l (replace dirs nombre r)


        toString :: Tree (MemoryNode, Maybe Nombre) -> String
        toString Leaf = ""
        toString (Node (MN{..},Nothing) Leaf Leaf) = "| Block size: " ++ show (2^value) ++ ", Status: Free    , Name: N/A\n"  
        toString (Node (MN{..},Just nombre) _ _)   = "| Block size: " ++ show (2^value) ++ ", Status: Occupied, Name: " ++ nombre ++ "\n"
        toString (Node _ l r) = toString l ++ toString r  

        modified = foldr (\(n,c) t -> replace c n t) coupled (Map.toList pCrumbs)
