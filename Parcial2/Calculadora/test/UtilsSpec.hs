{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module UtilsSpec where


import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Control.Monad 
import Infix
import Data.Foldable
import Utils
import Parser 

t = Terminal . LN


evalSpec :: Spec
evalSpec = describe "evaluation yields correct result." $ do 
    it "POST: 6 5 1 2 3 4 - - * - - = 4"  $ case eval Post "6 5 1 2 3 4 - - * - -" of Left _ -> False; Right n -> n == 4
    it "POST: 6 5 4 * / 2 + 1 - 2 - = -1" $ case eval Post "6 5 4 * / 2 + 1 - 2 -" of Left _ -> False; Right n -> n == (-1)
    it "PRE:  - 6 - 5 * 1 - 2 - 3 4 = 4"  $ case eval Pre  "- 6 - 5 * 1 - 2 - 3 4" of Left _ -> False; Right n -> n == 4
    it "PRE:  + / 6 * 5 4 - 2 - 1 2 = -1" $ case eval Pre  "+ / 6 * 5 4 - 2 - 1 2" of Left _ -> False; Right n -> n == 3

showSpec :: Spec
showSpec = describe "correctly parses to infix." $ do 
    it "POST: 6 5 1 2 3 4 - - * - - = 6 - (5 - 1 * (2 - (3 - 4)))"  $ case toString Post "6 5 1 2 3 4 - - * - -" of Left _ -> False; (Right n) -> n =="6 - (5 - 1 * (2 - (3 - 4)))"
    it "POST: 6 5 4 * / 2 + 1 - 2 - = 6 / (5 * 4) + 2 - 1 - 2"      $ case toString Post "6 5 4 * / 2 + 1 - 2 -" of Left _ -> False; (Right n) -> n =="6 / (5 * 4) + 2 - 1 - 2"
    it "PRE:  - 6 - 5 * 1 - 2 - 3 4 = 6 - (5 - 1 * (2 - (3 - 4)))"  $ case toString Pre  "- 6 - 5 * 1 - 2 - 3 4" of Left _ -> False; (Right n) -> n =="6 - (5 - 1 * (2 - (3 - 4)))"
    it "PRE:  - - + / 6 * 5 4 2 1 2 = 6 / (5 * 4) + 2 - 1 - 2"      $ case toString Pre  "- - + / 6 * 5 4 2 1 2" of Left _ -> False; (Right n) -> n =="6 / (5 * 4) + 2 - 1 - 2"

utilsSpec :: IO ()
utilsSpec = traverse_ hspec  [evalSpec,showSpec]


