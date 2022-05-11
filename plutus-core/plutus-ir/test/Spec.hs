{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import PlutusPrelude

import NamesSpec
import ParserSpec
import TransformSpec
import TypeSpec

import PlutusIR
import PlutusIR.Test

import PlutusCore qualified as PLC

import Test.Tasty
import Test.Tasty.Extras

import Flat (flat, unflat)

main :: IO ()
main = defaultMain $ runTestNestedIn ["plutus-ir/test"] tests

tests :: TestNested
tests = testGroup "plutus-ir" <$> sequence
    [ prettyprinting
    , parsing
    , lets
    , datatypes
    , recursion
    , serialization
    , errors
    , pure names
    , transform
    , types
    , typeErrors
    ]

prettyprinting :: TestNested
prettyprinting = testNested "prettyprinting"
    $ map (goldenPir id)
    [ "basic"
    , "maybe"
    ]

lets :: TestNested
lets = testNested "lets"
    [ goldenPlcFromPir "letInLet"
    , goldenPlcFromPir "letDep"
    ]

datatypes :: TestNested
datatypes = testNested "datatypes"
    [ goldenPlcFromPir "maybe"
    , goldenPlcFromPir "listMatch"
    , goldenPlcFromPirCatch "idleAll"
    , goldenPlcFromPirCatch "some"
    , goldenEvalPir "listMatchEval"
    ]

recursion :: TestNested
recursion = testNested "recursion"
    [ goldenPlcFromPir "even3"
    , goldenEvalPir "even3Eval"
    , goldenPlcFromPir "stupidZero"
    , goldenPlcFromPir "mutuallyRecursiveValues"
    , goldenEvalPir "errorBinding"
    ]

serialization :: TestNested
serialization = testNested "serialization"
    $ map (goldenPir roundTripPirTerm)
    [ "serializeBasic"
    , "serializeMaybePirTerm"
    , "serializeEvenOdd"
    , "serializeListMatch"
    ]

roundTripPirTerm :: Term TyName Name PLC.DefaultUni PLC.VCurrentDefaultFun a -> Term TyName Name PLC.DefaultUni PLC.VCurrentDefaultFun ()
roundTripPirTerm = decodeOrError . unflat . flat . void
  where
    decodeOrError (Right tm) = tm
    decodeOrError (Left err) = error (show err)

errors :: TestNested
errors = testNested "errors"
    [ goldenPlcFromPirCatch "mutuallyRecursiveTypes"
    , goldenPlcFromPirCatch "recursiveTypeBind"
    ]
