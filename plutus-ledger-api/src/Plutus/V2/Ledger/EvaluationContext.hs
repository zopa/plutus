{-# LANGUAGE DeriveAnyClass #-}

-- GHC is asked to do quite a lot of optimization in this module, so we're increasing the amount of
-- ticks for the simplifier not to run out of them.
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

module Plutus.V2.Ledger.EvaluationContext
    ( EvaluationContext (..)
    , DefaultMachineParameters
    , mkEvaluationContext
    , CostModelParams
    , isCostModelParamsWellFormed
    , toMachineParameters
    , costModelParamNames
    ) where

import Plutus.V1.Ledger.EvaluationContext hiding (mkEvaluationContext)
import PlutusCore as Plutus (UnliftingMode (..), Version (..), defaultCekCostModel)
import PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import PlutusCore.Evaluation.Machine.MachineParameters as Plutus

import GHC.Exts (inline)

mkMachineParametersFor :: UnliftingMode -> Plutus.CostModelParams -> Maybe DefaultMachineParameters
mkMachineParametersFor unlMode newCMP =
    inline Plutus.mkMachineParameters (Version () 2 0 0) unlMode <$>
        Plutus.applyCostModelParams Plutus.defaultCekCostModel newCMP
{-# INLINE mkMachineParametersFor #-}

-- See Note [Inlining meanings of builtins].
-- | Build the 'EvaluationContext'.
--
-- The input is a `Map` of strings to cost integer values (aka `Plutus.CostModelParams`, `Alonzo.CostModel`)
mkEvaluationContext :: Plutus.CostModelParams -> Maybe EvaluationContext
mkEvaluationContext newCMP =
    EvaluationContext
        <$> inline mkMachineParametersFor UnliftingImmediate newCMP
        <*> inline mkMachineParametersFor UnliftingDeferred newCMP
