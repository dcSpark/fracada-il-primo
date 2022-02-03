with import ./nix { };
(plutus-apps.plutus-apps.haskell.project.shellFor ({

  # Select packages who's dependencies should be added to the shell env
  packages = ps: [ ];

  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      plutus-tx
      plutus-tx-plugin
      plutus-ledger-api
      pab.plutus_ledger_with_docs
      plutus-pab
      plutus-core
      plutus-contract
      playground-common
      prettyprinter-configurable
      plutus-use-cases
      cabal-doctest
    ];

  withHoogle = true;

  # Extra haskell tools (arg passed on to mkDerivation)
  # Using the plutus.pkgs to use nixpkgs version from plutus (nixpkgs-unstable, mostly)
  propagatedBuildInputs = with pkgs; [
    # Haskell Tools
    stack
    cabal-install
    haskellPackages.ormolu
    entr
    git
    ghc
    nixfmt
    haskellPackages.apply-refact
    plutus.plutus.hlint

    plutus.plutus.haskell-language-server
    haskellPackages.retrie


    ### Example contracts
    plutus-apps.plutus-pab-examples
  ];

  buildInputs = (with plutus-apps.pkgs;
    [ zlib pkg-config libsodium-vrf R ]
    ++ (lib.optionals (!stdenv.isDarwin) [ systemd ]));

}))
