# Nix tools

This directory contains all of the nix helper functions used to build our dependencies.

# Formatting

Use nixfmt (provided by the shell) to format the nix sources.

# Niv dependency pinning

Use `niv` to update / modify nix dependencies.
To update all run `niv update`.

# Updating plutus && plutus apps

In order to update the `plutus` and `plutus-apps` revision, a few steps must be taken:

- `niv update plutus-apps -r <revision>` will update Nix's plutus-apps revision which will also produce a new shell
- also update the plutus version to the one referenced in plutus-apps
- You should update non-plutus entries in the sha256map in `./nix/haskell.nix`.
  You can copy from the plutus repo [here](https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix)
- Update the revision in `cabal.project` and paste the plutus and plutus-apps repo's `cabal.project` contents after the `*replace here*` separator

Now everything should be updated, good luck fixing compile errors!
