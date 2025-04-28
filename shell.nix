{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    yarn
    nodejs
    elmPackages.elm
    elmPackages.elm-live
    elmPackages.elm-format
    elmPackages.elm-review
  ];
}
