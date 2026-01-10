let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  # Add this if you are building a devShell in a flake. Usually, it's auto-detected
  # using lib.inNixShell, but that doesn't work in flakes
  # returnShellEnv = true;
in
pkgs.haskellPackages.developPackage {
  root = ./.;
}
