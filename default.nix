{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and bcc-nix:
# nix build -f default.nix bcc-shell '{
#   bcc-nix = ../bcc-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (bcc-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.bccNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages bccShellHaskellPackages);

  uploadCoverallsScript = pkgSet:
    let
      projectPkgs = selectProjectPackages pkgSet;
      projectCoverageReport = pkgSet.projectCoverageReport;
    in writeShellScriptBin "uploadCoveralls.sh" ''
      ${commonLib.hpc-coveralls}/bin/hpc-coveralls all \
        ${concatStringsSep "\n  " (mapAttrsToList (_: p: "--package-dir .${p.src.origSubDir} \\") projectPkgs)}
        --hpc-dir ${projectCoverageReport}/share/hpc/vanilla \
        --coverage-mode StrictlyFullLines \
        --repo-token=$COVERALLS_REPO_TOKEN
    '';

  self = {
    inherit bccShellHaskellPackages;
    inherit haskellPackages hydraEvalErrors;

    inherit (haskellPackages.bcc-shell.identifier) version;
    # Grab the executable component of our package.
    inherit (haskellPackages.bcc-shell.components.exes) node-ipc;
    inherit (haskellPackages.bcc-launcher.components.exes) bcc-launcher;

    inherit (pkgs.bccNix) checkCabalProject;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    inherit (commonLib) hpc-coveralls;
    uploadCoverallsScript = uploadCoverallsScript bccShellHaskellPackages;

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in self
