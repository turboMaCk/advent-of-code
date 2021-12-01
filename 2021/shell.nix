{ pkgs ? import <nixpkgs> {} }:
let
  haskellEnv =
    pkgs.haskell.packages.ghc8104.ghcWithPackages
      (hsPackages: with hsPackages; [
        unordered-containers
        text
        vector
      ]);

  measure = pkgs.writeScriptBin "measure" ''
    $1
    PID=$!
    psrecord "{$PID}" --plot "res.png"
  '';
in
with pkgs;
mkShell {
  inputsFrom = [ haskellEnv ];
  buildInputs = [ measure rustc ];
}
