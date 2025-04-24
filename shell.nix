{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    elmPackages.elm
    elmPackages.elm-format
  ];
}
