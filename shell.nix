{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    just
    elmPackages.elm
    elmPackages.elm-format
  ];
}
