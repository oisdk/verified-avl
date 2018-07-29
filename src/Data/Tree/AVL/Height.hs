module Data.Tree.AVL.Height where

import Data.Kind

data Height
    = Z
    | S Height

data Balance :: Height -> Height -> Height -> Type where
        L :: Balance (S n) n (S n)
        O :: Balance n n n
        R :: Balance n (S n) (S n)

balr :: Balance x y z -> Balance z x z
balr L = O
balr O = O
balr R = L
{-# INLINE balr #-}

ball :: Balance x y z -> Balance y z z
ball L = R
ball O = O
ball R = O
{-# INLINE ball #-}
