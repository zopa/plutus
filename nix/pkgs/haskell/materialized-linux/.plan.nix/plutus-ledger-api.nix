{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-ledger-api"; version = "1.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones, Jann Mueller";
      homepage = "";
      url = "";
      synopsis = "Interface to the Plutus ledger for the Cardano ledger.";
      description = "Interface to the Plutus scripting support for the Cardano ledger.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          ];
        buildable = true;
        modules = [
          "Codec/CBOR/Extras"
          "Data/Either/Extras"
          "Prettyprinter/Extras"
          "PlutusLedgerApi/V1"
          "PlutusLedgerApi/V1/Address"
          "PlutusLedgerApi/V1/Bytes"
          "PlutusLedgerApi/V1/Contexts"
          "PlutusLedgerApi/V1/Credential"
          "PlutusLedgerApi/V1/Crypto"
          "PlutusLedgerApi/V1/DCert"
          "PlutusLedgerApi/V1/Interval"
          "PlutusLedgerApi/V1/Scripts"
          "PlutusLedgerApi/V1/Tx"
          "PlutusLedgerApi/V1/Time"
          "PlutusLedgerApi/V1/Value"
          "PlutusLedgerApi/V1/EvaluationContext"
          "PlutusLedgerApi/V2"
          "PlutusLedgerApi/V2/Contexts"
          "PlutusLedgerApi/V2/Tx"
          "PlutusLedgerApi/Common"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "plutus-ledger-api-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ];
          buildable = true;
          modules = [
            "PlutusLedgerApi/Test/Examples"
            "PlutusLedgerApi/Test/EvaluationContext"
            "PlutusLedgerApi/Test/Scripts"
            ];
          hsSourceDirs = [ "testlib" ];
          };
        };
      tests = {
        "plutus-ledger-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            ];
          buildable = true;
          modules = [ "Spec/Interval" "Spec/Eval" "Spec/Builtins" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../plutus-ledger-api; }