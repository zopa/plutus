{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text.IO (readFile)
import Prelude hiding (readFile)
import Test.Tasty.Golden (findByExtension)

main :: IO ()
main = do
    inputFiles <- findByExtension [".uplc"] "plutus-conformance/uplc/evaluation/textual-inputs"
    outputFiles <- findByExtension [".expected"] "plutus-conformance/uplc/evaluation/textual-outputs"
    _lProg <- traverse readFile inputFiles
    _lEvaluatedRes <- traverse readFile outputFiles
    pure ()

