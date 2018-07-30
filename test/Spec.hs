{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Data.Tree.AVL.Map   (Map)
import qualified Data.Tree.AVL.Map   as Map

import           Control.Applicative

readableString :: Gen String
readableString = Gen.list (Range.linear 0 10) Gen.lower


avl :: Gen (Map Char String)
avl =
    Map.fromList <$>
    Gen.list
         (Range.linear 0 1000)
         (liftA2 (,) Gen.lower readableString)

prop_insert :: Property
prop_insert = property $ do
    xs <- forAll avl
    k  <- forAll Gen.lower
    v  <- forAll readableString
    Map.lookup k (Map.insert k v xs) === Just v

prop_delete :: Property
prop_delete = property $ do
    xs <- forAll avl
    k  <- forAll Gen.lower
    Map.lookup k (Map.delete k xs) === Nothing

main :: IO Bool
main = checkParallel $$(discover)
