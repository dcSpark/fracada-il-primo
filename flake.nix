{
  description = "fracada-il-primo";

  inputs = {
    iohk-nix.follows = "plutus-simple-model/tooling/iohk-nix";
    haskell-nix.follows = "plutus-simple-model/tooling/haskell-nix";
    nixpkgs.follows = "plutus-simple-model/nixpkgs";
    tooling.follows = "plutus-simple-model/tooling";
    plutarch.follows = "plutus-simple-model/plutarch";
    #    plutus.flake = false;
    plutus-simple-model.url = "github:mlabs-haskell/plutus-simple-model?rev=450e278a819bf2955828726d02e239d233c36cba";
  };

  outputs = inputs@{ self, tooling, plutarch, nixpkgs, ... }:
      tooling.lib.mkFlake { inherit self; }
        {
          imports = [
            (tooling.lib.mkHaskellFlakeModule1 {
              project.src = ./.;
              project.extraHackage = [
                "${plutarch}"
                "${plutarch}/plutarch-extra"
              ];
              toHaddock = [
                "plutarch"
                "plutarch-extra"
                "cardano-crypto"
                "cardano-ledger-alonzo"
                "cardano-ledger-babbage"
                "cardano-ledger-core"
              ];

            })
          ];
        };
}