{ pkgs, ... }:

let markdown-toc-emacs = pkgs.emacsWithPackages (epkgs:
      (with epkgs.melpaStablePackages; [
        markdown-mode s dash
      ]) ++ (with pkgs; [
        emacs markdown-toc
      ])
    );
in pkgs.stdenv.mkDerivation {
  name = "markdown-toc";
  buildInputs = with pkgs; [
    markdown-toc-emacs
    pkgs.gitAndTools.hub
    cask
  ];
  src = null;
}
