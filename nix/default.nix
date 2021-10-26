#TODO review overalys and add bcc-haskell-nix
tac{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  bcccoinNix = import sources.bcccoin-nix {};
  haskellNix = import sources."haskell.nix" {};
  # use our own nixpkgs if it exists in our sources,
  # otherwise use bcccoinNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then (builtins.trace "Not using The-Blockchain-Company default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else (builtins.trace "Using The-Blockchain-Company default nixpkgs"
      bcccoinNix.nixpkgs);
#TODO
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/The-Blockchain-Company/haskell.nix)
    haskellNix.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ bcccoinNix.overlays.haskell-nix-extra
    # bcccoinNix: nix utilities and niv:
    ++ bcccoinNix.overlays.bcccoinNix
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {

        # commonLib: mix pkgs.lib with bcccoin-nix utils and our own:
        commonLib = lib // bcccoinNix // bcccoinNix.bccLib
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources and overlays
          // { inherit overlays sources; };

        svcLib = import ./svclib.nix { inherit pkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
      # stack needs to be version 1.9.3, because versions greater than
      # this can't be re-execed in a nix shell:
      #
      # https://github.com/commercialhaskell/stack/issues/5000
      #
      # i.e. "runCoveralls" fails with stack > 1.9.3
      (self: super: {
        stack_1_9_3 = (import sources."nixpkgs-19.03" {}).stack;
      })
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
