{-# LANGUAGE  FlexibleContexts  #-}

module Controller where

import Lexer (parseEntry)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad
import Control.Applicative
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph 
import Data.Foldable (traverse_)
import Control.Monad.Except
import Debug.Trace

newtype ClassId  = CID {getCID :: String} deriving (Eq,Ord)
newtype MethodId = MID {getMID :: String} deriving (Eq,Ord)
data MethodState = MST {getMSTID :: MethodId, getMemberClassId :: ClassId}  deriving Show

instance Eq MethodState where
    (MST name _) == (MST name' _) = name == name'

instance Ord MethodState where
    (MST name _) <= (MST name' _) = name <= name'

instance Show ClassId where
    show (CID n) = n

instance Show MethodId where
    show (MID n) = n

data Class = Class 
    { selfID    :: ClassId
    , methods   :: Set MethodState
    }

data InnerSt = IST
    { graph :: Gr Class ()
    , trans :: Map ClassId Node
    }


initialST :: InnerSt
initialST = IST Data.Graph.Inductive.Graph.empty Map.empty

data MError = BadLex | AlreadyExists | InexistentSuper String | InexistentClass | RepeatedDefinitions | Cycle



classExist :: MonadError MError m => ClassId -> StateT InnerSt m ()
classExist c = gets trans >>= \t -> when (Map.member c t) $ throwError AlreadyExists 

classNotExist :: MonadError MError m => ClassId -> StateT InnerSt m ()
classNotExist c = gets trans >>= \t -> unless (Map.member c t) $ throwError (InexistentSuper $ getCID c)

repeatedMethods :: (MonadError MError m) => [MethodId] -> StateT InnerSt m ()
repeatedMethods ms = when (length ms /= length (Set.fromList ms)) $ throwError RepeatedDefinitions

insertClass :: MonadError MError m => ClassId -> [ClassId] -> [MethodId] -> StateT InnerSt m ()
insertClass c supers ms = do

    classExist c
    traverse_ classNotExist supers 
    repeatedMethods ms

    g          <- gets graph
    superNodes <- gets ((\m -> [(x,m ! x) | x <- supers]) . trans)
    parentMethods <- forM superNodes $ \(name,id) -> gets ( methods . lab' . flip context id . graph)


    let methods  = Set.unions $ Set.fromList (flip MST c <$> ms) :  reverse parentMethods
        newClass = Class c methods 
        newID    = if not $ isEmpty g then (1+) $ snd $ nodeRange g else 0
        newNode  = (newID, newClass)
        newEs    = fmap (\(_,s) -> (s,newID,()) ) superNodes
        newG     = insEdges newEs $ insNode newNode g

    modify' (\ist -> ist{graph=newG,trans = Map.insert c newID (trans ist)})


describe :: MonadError MError m => ClassId -> StateT InnerSt m (Set MethodState)
describe c = do
    (dict,g) <- gets (\s -> (trans s,graph s))

    when (c `Map.notMember` dict ) $ throwError InexistentClass

    let k = dict ! c 

    gets (methods . (\(_,_,m,_) -> m) . flip context k . graph)


prettyShowMS :: MethodState -> String 
prettyShowMS (MST name c) = show name ++ " -> " ++ show c ++ " :: " ++ show name


prettyShowMST :: Foldable t => t MethodState -> String
prettyShowMST = foldMap ((++ "\n") . prettyShowMS) 

