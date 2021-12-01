{ pkgs ? import <nixpkgs> {} }:
let
  haskellEnv =
    pkgs.haskell.packages.ghc8104.ghcWithPackages
      (hsPackages: with hsPackages; [
        unordered-containers
        text
        vector
      ]);
in
pkgs.mkShell {
  inputsFrom = with pkgs; [ haskellEnv ];
  buildInputs = [];
}
