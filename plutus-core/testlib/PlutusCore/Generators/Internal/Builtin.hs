{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusCore.Generators.Internal.Builtin (
    genArg,
    genConstant,
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
import Data.Kind qualified as GHC
import Data.Text (Text)
import Data.Type.Equality
import Data.Typeable (splitTyConApp)
import Hedgehog hiding (Opaque, Var, eval)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Type.Reflection

data SomeGen = forall a. DefaultUni `Contains` a => SomeGen (Gen a)

genConstant :: forall (a :: GHC.Type). TypeRep a -> SomeGen
genConstant tr
    | Just HRefl <- eqTypeRep tr (typeRep @()) = SomeGen $ pure ()
    | Just HRefl <- eqTypeRep tr (typeRep @Integer) = SomeGen genInteger
    | Just HRefl <- eqTypeRep tr (typeRep @Int) = SomeGen genInteger
    | Just HRefl <- eqTypeRep tr (typeRep @Bool) = SomeGen Gen.bool
    | Just HRefl <- eqTypeRep tr (typeRep @BS.ByteString) = SomeGen genByteString
    | Just HRefl <- eqTypeRep tr (typeRep @Text) = SomeGen genText
    | Just HRefl <- eqTypeRep tr (typeRep @Data) = SomeGen $ genData 5
    | trPair `App` tr1 `App` tr2 <- tr
    , Just HRefl <- eqTypeRep trPair (typeRep @(,)) =
        case (genConstant tr1, genConstant tr2) of
            (SomeGen g1, SomeGen g2) -> SomeGen $ (,) <$> g1 <*> g2
    | trList `App` trElem <- tr
    , Just HRefl <- eqTypeRep trList (typeRep @[]) =
        case genConstant trElem of
            SomeGen genElem -> SomeGen $ Gen.list (Range.linear 0 10) genElem
    | trSomeConstant `App` _ `App` trEl <- tr
    , Just HRefl <- eqTypeRep trSomeConstant (typeRep @SomeConstant) =
          genConstant trEl
    | trOpaque `App` _ `App` trEl <- tr
    , Just HRefl <- eqTypeRep trOpaque (typeRep @Opaque) =
          genConstant trEl
    | trTyVarRep `App` _ <- tr
    , Just HRefl <- eqTypeRep trTyVarRep (typeRep @(TyVarRep @GHC.Type)) =
          -- In the current implementation, all type variables are instantiated
          -- to `Integer` (TODO: change this?).
          genConstant $ typeRep @Integer
    | otherwise =
        error $
            "genConstant: I don't know how to generate constant of this type: " <> show tr

genArg :: forall (a :: GHC.Type). TypeRep a -> Gen (Term TyName Name DefaultUni DefaultFun ())
genArg tr =
    case genConstant tr of
        SomeGen gen -> Constant () . someValue <$> gen

-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> Gen.sample (genArg (typeRep @(SomeConstant DefaultUni [TyVarRep @GHC.Type ('TyNameRep "a" 0)])))
-- Constant () (Some (ValueOf list (integer) [-4070851582659524810,-2978682499237489585]))

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
