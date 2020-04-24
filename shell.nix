with import <nixpkgs> {};

let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
in stdenv.mkDerivation {
  name = "markdown-toc-env";
  buildInputs = [
    pkgs.cask
    pkgs.gitAndTools.hub
  ];
  src = null;
}
