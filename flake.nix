{
  description = "fracada-il-primo";

  inputs = {

    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false;
    plutarch.url = "github:plutonomicon/plutarch-plutus";
    #    plutus.flake = false;
    plutus-simple-model.url = "github:mlabs-haskell/plutus-simple-model?rev=450e278a819bf2955828726d02e239d233c36cba";
    plutus-simple-model.inputs.plutus.follows = "plutus";
  };

  outputs = { self, nixpkgs, haskell-nix, plutus-simple-model, iohk-nix, plutarch, ... }:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit system;
        };

      nixpkgsFor' = system: import nixpkgs { inherit system; };

      compiler-nix-name = "ghc8107";

      myhackages =
        let i = plutus-simple-model.inputs; in
        system: compiler-nix-name: plutarch.inputs.haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name (
        [
          "${plutus-simple-model}"
          "${i.cardano-base}/base-deriving-via"
          "${i.cardano-base}/binary"
          "${i.cardano-base}/binary/test"
          "${i.cardano-base}/cardano-crypto-class"
          "${i.cardano-base}/cardano-crypto-praos"
          "${i.cardano-base}/cardano-crypto-tests"
          "${i.cardano-base}/measures"
          "${i.cardano-base}/orphans-deriving-via"
          "${i.cardano-base}/slotting"
          "${i.cardano-base}/strict-containers"
          "${i.cardano-crypto}"
          "${i.cardano-ledger}/eras/alonzo/impl"
          "${i.cardano-ledger}/eras/babbage/impl"
          "${i.cardano-ledger}/eras/byron/chain/executable-spec"
          "${i.cardano-ledger}/eras/byron/crypto"
          "${i.cardano-ledger}/eras/byron/crypto/test"
          "${i.cardano-ledger}/eras/byron/ledger/executable-spec"
          "${i.cardano-ledger}/eras/byron/ledger/impl"
          "${i.cardano-ledger}/eras/byron/ledger/impl/test"
          "${i.cardano-ledger}/eras/shelley/impl"
          "${i.cardano-ledger}/eras/shelley-ma/impl"
          "${i.cardano-ledger}/eras/shelley/test-suite"
          "${i.cardano-ledger}/libs/cardano-data"
          "${i.cardano-ledger}/libs/cardano-ledger-core"
          "${i.cardano-ledger}/libs/cardano-ledger-pretty"
          "${i.cardano-ledger}/libs/cardano-protocol-tpraos"
          "${i.cardano-ledger}/libs/non-integral"
          "${i.cardano-ledger}/libs/set-algebra"
          "${i.cardano-ledger}/libs/small-steps"
          "${i.cardano-ledger}/libs/small-steps-test"
          "${i.cardano-ledger}/libs/vector-map"
          "${i.cardano-prelude}/cardano-prelude"
          "${i.cardano-prelude}/cardano-prelude-test"
          "${i.flat}"
          "${i.goblins}"
          "${i.plutus}/plutus-core"
          "${i.plutus}/plutus-ledger-api"
          "${i.plutus}/plutus-tx"
          "${i.plutus}/plutus-tx-plugin"
          "${i.plutus}/prettyprinter-configurable"
          "${i.plutus}/stubs/plutus-ghc-stub"
          "${i.plutus}/word-array"
          "${i.Win32-network}"
        ]
      );

      haskellModules = [
        ({ pkgs, ... }:
          {
            packages = {
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              plutus-simple-model.doHaddock = false;
            };
          }
        )
      ];

      hackagesFor = system:
        let hackages = myhackages system compiler-nix-name;
        in {
          inherit (hackages) extra-hackages extra-hackage-tarballs;
          modules = haskellModules ++ hackages.modules;
        };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          pkgSet = pkgs.haskell-nix.cabalProject' ({
              src = ./.;
              compiler-nix-name = compiler-nix-name;
              inherit (hackagesFor system) extra-hackages extra-hackage-tarballs modules;
              cabalProjectLocal =
                ''
                  allow-newer:
                      *:aeson
                    , size-based:template-haskell
                    , *:ral
                    , *:fin
                    , *:some

                  constraints:
                      aeson >= 2
                    , hedgehog >= 1.1
                    , flat >= 0.4.5
                    , algebraic-graphs < 0.7
                ''
              ;
              shell = {
                withHoogle = true;
                exactDeps = true;

                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.hlint
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.nixpkgs-fmt
                ];

                # tools.haskell-language-server = { };
              };
            });
          in pkgSet;
    in {
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "fracada-il-primo:lib:fracada-il-primo";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);
      hackages = perSystem (system: hackagesFor system);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check" {
          nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ builtins.attrValues self.flake.${system}.packages
            ++ [ self.flake.${system}.devShell.inputDerivation ];
        } "touch $out");

      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      #
      # Includes all of the packages in the `checks`, otherwise only the
      # test suite would be included
      checks = perSystem (system: self.flake.${system}.checks);
    };
}