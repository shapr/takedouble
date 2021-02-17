{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Takedouble

-- prop_test :: Property
-- prop_test = property $ do
--   doTakedouble === "Takedouble"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
