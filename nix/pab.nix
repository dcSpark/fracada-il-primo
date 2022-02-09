{ plutus-apps, pkgs ? plutus-apps.pkgs }: rec {
  plutus_ledger_with_docs =
    plutus-apps.plutus-apps.haskell.packages.plutus-ledger.components.library.override {
      doHaddock = true;
      configureFlags = [ "-f defer-plugin-errors" ];
    };
}
