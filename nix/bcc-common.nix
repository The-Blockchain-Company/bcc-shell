# Imports the bcc-nix library.
# The version can be overridden for debugging purposes by setting
# NIX_PATH=bcc_nix=/path/to/bcc-nix
import (
  let try = builtins.tryEval <bcc_nix>;
  in if try.success
  then builtins.trace "using host <bcc_nix>" try.value
  else
    let
      spec = builtins.fromJSON (builtins.readFile ./bcc-nix-src.json);
    in builtins.fetchTarball {
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
      inherit (spec) sha256;
    }) {}