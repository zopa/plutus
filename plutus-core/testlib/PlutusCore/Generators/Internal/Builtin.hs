{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusCore.Generators.Internal.Builtin (
    genArg,
    genInteger,
    genByteString,
    genText,
    genData,
    genI,
    genB,
    genList,
    genMap,
    genConstr,
    matchTyCon,
) where

import PlutusPrelude

import PlutusCore
import PlutusCore.Builtin
import PlutusCore.Data (Data (..))

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Type.Equality
import Data.Typeable (splitTyConApp)
import Hedgehog hiding (Opaque, Var, eval)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PlutusCore.Generators.AST qualified as AST
import Type.Reflection

genConstant :: TypeRep a -> Gen a
genConstant (eqTypeRep (typeRep @()) -> Just HRefl) = pure ()
genConstant (eqTypeRep (typeRep @Integer) -> Just HRefl) = genInteger
genConstant ((eqTypeRep (typeRep @(,)) -> Just HRefl) `App` tr1 `App` tr2) =
    (,) <$> genConstant tr1 <*> genConstant tr2
genConstant ((eqTypeRep (typeRep @[]) -> Just HRefl) `App` trElem) =
    Gen.list (Range.linear 0 10) $ genConstant trElem
genConstant _ = error "FIXME: ADD MORE CASES"
-- genConstant tr =
--     fail $
--         "genConstant: I don't know how to generate builtin arguments of this type: " ++ show tr

genArg
    :: term ~ Term TyName Name DefaultUni DefaultFun ()
    => MakeKnownIn DefaultUni term a => TypeRep a -> Gen term
genArg (matchTyCon @Opaque -> Just _)       = AST.runAstGen AST.genTerm
genArg (matchTyCon @SomeConstant -> Just _) = error "FIXME: IMPLEMENT"
genArg tr                                   = genConstant tr >>= reoption . makeKnownOrFail

-- | If the given `TypeRep`'s `TyCon` is @con@, return its type arguments.
matchTyCon :: forall con a. (Typeable con) => TypeRep a -> Maybe [SomeTypeRep]
matchTyCon tr = if con == con' then Just args else Nothing
  where
    (con, args) = splitTyConApp (SomeTypeRep tr)
    con' = typeRepTyCon (typeRep @con)

----------------------------------------------------------
-- Generators

genInteger :: Gen Integer
genInteger = fromIntegral @Int64 <$> Gen.enumBounded

genByteString :: Gen BS.ByteString
genByteString = Gen.utf8 (Range.linear 0 100) Gen.enumBounded

genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.enumBounded

genData :: Int -> Gen Data
genData depth =
    Gen.choice $
        [genI, genB]
            <> [ genRec | depth > 1, genRec <-
                                        [ genList (depth - 1)
                                        , genMap (depth - 1)
                                        , genConstr (depth - 1)
                                        ]
               ]

genI :: Gen Data
genI = I <$> genInteger

genB :: Gen Data
genB = B <$> genByteString

genList :: Int -> Gen Data
genList depth = List <$> Gen.list (Range.linear 0 5) (genData (depth - 1))

genMap :: Int -> Gen Data
genMap depth =
    Map
        <$> Gen.list
            (Range.linear 0 5)
            ((,) <$> genData (depth - 1) <*> genData (depth - 1))

genConstr :: Int -> Gen Data
genConstr depth =
    Constr <$> genInteger
        <*> Gen.list
            (Range.linear 0 5)
            (genData (depth - 1))
