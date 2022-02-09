{ sourcesFile ? ./sources.json
, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, plutus-apps ? import sources.plutus-apps { }
, deferPluginErrors ? true
, doCoverage ? false
, typedRawData ? false
}:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus plutus-apps deferPluginErrors
      doCoverage typedRawData;
  };
in
rec {

  inherit (project.wingriders.components) library;
  inherit (project.wingriders.components.exes) report;
}
