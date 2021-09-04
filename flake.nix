{
  description = "markdown-toc flake";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
      follows = "nix/nixpkgs";
    };

    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
      ref = "master";
    };
  };

  outputs = { self, nix, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let lib = nixpkgs.lib;
            pkgs = nixpkgs.legacyPackages.${system};
            pname = "markdown-toc";
            version = "0.1.5";
        in rec {
          packages."${system}" = {
            markdown-toc = pkgs.stdenv.mkDerivation {
              inherit pname version;
              src = ./.;
              buildInputs = with pkgs.emacs.pkgs; [
                emacs s dash markdown-mode
              ];
              unpackPhase = ''
                cp $src/${pname}.el .
              '';
              buildPhase = ''
                emacs -L . --batch -f batch-byte-compile *.el
              '';
              installPhase =
                let install-dir = "$out/share/emacs/site-lisp/elpa/${pname}-${version}/"; in
                ''
                mkdir -p ${install-dir}
                cp -v *.el *.elc ${install-dir}
              '';

              doCheck = false;

              meta = {
                description = "A simple TOC generator for markdown file.";
                homepage = https://github.com/ardumont/markdown-toc;
                license = lib.licenses.gpl2;
                maintainers = with lib.maintainers; [ ardumont ];
              };
            };
          };

          devShell = import ./shell.nix { pkgs = pkgs // packages."${system}"; };

          defaultPackage = packages."${system}".markdown-toc;
        });
}
