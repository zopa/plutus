{-# LANGUAGE TypeOperators #-}

-- See CostModelGeneration.md
module Main (main) where

import CriterionExtensions (criterionMainWith)

import Benchmarks.Bool qualified
import Benchmarks.ByteStrings qualified
import Benchmarks.CryptoAndHashes qualified
import Benchmarks.Data qualified
import Benchmarks.Integers qualified
import Benchmarks.Lists qualified
import Benchmarks.Misc qualified
import Benchmarks.Nops qualified
import Benchmarks.Pairs qualified
import Benchmarks.Strings qualified
import Benchmarks.Tracing qualified
import Benchmarks.Unit qualified

import Criterion.Main
import Criterion.Types as C
import System.Random (getStdGen)

---------------- Miscellaneous ----------------

{- Creates the .csv file consumed by create-cost-model. The data in this file is
   the time taken for all the builtin operations, as measured by criterion.  See
   also 'CostModelGeneration.md'. -}

{- Experimentation and examination of implementations suggests that the cost
   models for certain builtins can be re-used for others, and we do this in
   models.R.  Specifically, we re-use the cost models for the functions on the
   left below for the functions on the right as well.  Because of this we don't
   benchmark the functions on the right; the benchmarks take a long time to run,
   so this speeds things up a lot.

   AddInteger:            SubtractInteger
   DivideInteger:         RemainderInteger, QuotientInteger, ModInteger
-}

main :: IO ()
main = do
  criterionMainWith True defaultConfig Benchmarks.Data.makeBenchmarks

