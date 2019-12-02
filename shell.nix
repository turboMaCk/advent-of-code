{ pkgs ? import <nixpkgs> {} }:
let
  haskellEnv =
    pkgs.haskell.packages.ghc863.ghcWithPackages
      (hsPackages: with hsPackages; [
      ]);
in
pkgs.mkShell {
  inputsFrom = with pkgs; [ haskellEnv ];
  buildInputs = [];
}
