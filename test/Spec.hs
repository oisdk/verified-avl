{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Data.Tree.AVL.Map   (Map)
import qualified Data.Tree.AVL.Map   as Map

import           Control.Applicative
import           Data.Functor.Const
import           Data.Functor.Identity

readableString :: Gen String
readableString = Gen.list (Range.linear 0 10) Gen.lower

avl :: Gen (Map Char String)
avl =
    Map.fromList <$>
    Gen.list
         (Range.linear 0 1000)
         (liftA2 (,) Gen.lower readableString)

lensLookup :: Ord k => k -> Map k v -> Maybe v
lensLookup k = getConst . Map.alterF k Const

lensDelete :: Ord k => k -> Map k v -> Map k v
lensDelete k = runIdentity . Map.alterF k (const (Identity Nothing))

prop_insert :: Property
prop_insert = property $ do
    xs <- forAll avl
    k  <- forAll Gen.lower
    v  <- forAll readableString
    Map.lookup k (Map.insert k v xs) === Just v
    lensLookup k (Map.insert k v xs) === Just v

prop_delete :: Property
prop_delete = property $ do
    xs <- forAll avl
    k  <- forAll Gen.lower
    Map.lookup k (Map.delete k xs) === Nothing
    Map.lookup k (lensDelete k xs) === Nothing

main :: IO Bool
main = checkParallel $$(discover)
