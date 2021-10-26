# our packages overlay
pkgs: _: with pkgs; {
  bccShellHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
