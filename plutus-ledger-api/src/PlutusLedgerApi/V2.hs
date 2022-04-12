{-# LANGUAGE DerivingStrategies #-}
{- |
The interface to Plutus V2 for the ledger.
-}
module PlutusLedgerApi.V2 (
    -- * Scripts
    SerializedScript
    , Script
    , fromCompiledCode
    -- * Validating scripts
    , isScriptWellFormed
    -- * Running scripts
    , evaluateScriptRestricting
    , evaluateScriptCounting
    -- ** Protocol version
    , ProtocolVersion (..)
    -- ** Verbose mode and log output
    , VerboseMode (..)
    , LogOutput
    -- * Costing-related types
    , ExBudget (..)
    , ExCPU (..)
    , ExMemory (..)
    , SatInt
    -- ** Cost model
    , EvaluationContext
    , mkEvaluationContext
    , CostModelParams
    , isCostModelParamsWellFormed
    , costModelParamNames
    -- * Context types
    , ScriptContext(..)
    , ScriptPurpose(..)
    -- ** Supporting types used in the context types
    -- *** ByteStrings
    , BuiltinByteString
    , toBuiltin
    , fromBuiltin
    -- *** Bytes
    , LedgerBytes (..)
    , fromBytes
    -- *** Certificates
    , DCert(..)
    -- *** Credentials
    , StakingCredential(..)
    , Credential(..)
    -- *** Value
    , Value (..)
    , CurrencySymbol (..)
    , TokenName (..)
    , singleton
    , unionWith
    , adaSymbol
    , adaToken
    -- *** Time
    , POSIXTime (..)
    , POSIXTimeRange
    -- *** Types for representing transactions
    , Address (..)
    , PubKeyHash (..)
    , TxId (..)
    , TxInfo (..)
    , TxOut(..)
    , TxOutRef(..)
    , TxInInfo(..)
    , OutputDatum (..)
    -- *** Intervals
    , Interval (..)
    , Extended (..)
    , Closure
    , UpperBound (..)
    , LowerBound (..)
    , always
    , from
    , to
    , lowerBound
    , upperBound
    , strictLowerBound
    , strictUpperBound
    -- *** Association maps
    , Map
    , fromList
    -- *** Newtypes for script/datum types and hash types
    , Validator (..)
    , mkValidatorScript
    , unValidatorScript
    , ValidatorHash (..)
    , MintingPolicy (..)
    , mkMintingPolicyScript
    , unMintingPolicyScript
    , MintingPolicyHash (..)
    , StakeValidator (..)
    , mkStakeValidatorScript
    , unStakeValidatorScript
    , StakeValidatorHash (..)
    , Redeemer (..)
    , RedeemerHash (..)
    , Datum (..)
    , DatumHash (..)
    , ScriptHash (..)
    -- * Data
    , Data (..)
    , BuiltinData (..)
    , ToData (..)
    , FromData (..)
    , UnsafeFromData (..)
    , toData
    , fromData
    , dataToBuiltinData
    , builtinDataToData
    -- * Errors
    , EvaluationError (..)
) where

import PlutusLedgerApi.V1 hiding (ScriptContext (..), TxInInfo (..), TxInfo (..), TxOut (..))
import PlutusLedgerApi.V1.Scripts (ScriptHash (..))
import PlutusLedgerApi.V2.Contexts
import PlutusLedgerApi.V2.Tx (OutputDatum (..))
import PlutusTx.AssocMap (Map, fromList)
