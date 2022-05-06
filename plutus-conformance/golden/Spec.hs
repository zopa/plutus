-- | This suite is for easy addition of tests. When run,
-- output files will be added to all tests that had no output files.
-- You are advised to manually check that the output is correct.

{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Common
import Data.Text qualified as T
import PlutusCore.Error
import PlutusCore.Evaluation.Result (EvaluationResult (..))
import System.Directory
import System.Environment
import Test.Tasty.Golden (findByExtension)
import Text.Megaparsec

main :: IO ()
main = do
    args <- getArgs
    case args of
      [ext,dir,action] -> do
          allInputFiles <- findByExtension [ext] dir
          -- only choose the ones without an output file, so as to not edit the ones already with outputs
          inputFiles <- sequenceA $
                [do
                    noOutput <- findFile [dir] (testIn <> ".expected")
                    case noOutput of
                        Just _  -> pure []
                        Nothing -> pure [testIn] | testIn <- allInputFiles]
          case action of
            "eval" -> do
              mapM_
                (\inputFile -> do
                  inputStr <- readFile inputFile
                  let parsed = parseWOPos inputFile $ T.pack inputStr
                      outFilePath = inputFile <> "expected"
                  case parsed of
                    Left (ParseErrorB peb) -> do
                      -- warn the user that the file failed to parse
                      putStr $ inputFile <> " failed to parse. Error written to " <> outFilePath
                      writeFile outFilePath (errorBundlePretty peb)
                    Right pro -> do
                      res <- evalUplcProg pro
                      output <- case res of
                        EvaluationSuccess prog -> pure prog
                        EvaluationFailure      -> error "TODO"
                      writeFile (inputFile <> ".expected") (show output))
                (concat inputFiles)
            "typecheck" ->
              error "typechecking has not been implemented yet. Only evaluation tests (eval) are supported."
            _ -> error $
              "Unsupported test " <> show action <>
              ". Please choose either eval (for evaluation tests) or typecheck (for typechecking tests)."
      _ -> error $
            "Please input the 3 arguments for running the golden tests: " <>
            "(1) file extension to be searched " <>
            "(2) directory to be searched " <>
            "(3) eval (for evaluation tests) or typecheck (for typechecking tests). "
