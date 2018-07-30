module Main (main) where

import Test.DocTest

main :: IO ()
main =
    doctest
        [ "-isrc"
        , "src/"
        , "-XDataKinds"
        , "-XKindSignatures"
        , "-XGADTs"
        , "-XRankNTypes"
        , "-XScopedTypeVariables"
        , "-XLambdaCase"
        , "-XStandaloneDeriving"
        , "-XDeriveFunctor"
        , "-XDeriveFoldable"
        , "-XDeriveTraversable"]
