module PlutusLedgerApi.Test.EvaluationContext
    ( costModelParamsForTesting
    , evalCtxForTesting
    ) where

import PlutusCore as Plutus
import PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import PlutusLedgerApi.V1.EvaluationContext

import Data.Maybe

-- | The raw cost model params, only to be used for testing purposes.
costModelParamsForTesting :: Plutus.CostModelParams
costModelParamsForTesting = fromJust Plutus.defaultCostModelParams

-- | only to be for testing purposes: make an evaluation context by applying an empty set of protocol parameters
evalCtxForTesting :: EvaluationContext
evalCtxForTesting = fromJust $ mkEvaluationContext mempty
