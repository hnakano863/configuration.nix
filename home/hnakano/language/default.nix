{ pkgs, pkgs-unstable, ... }:
{
  imports = [
    ./julia
    ./python
  ];

  home.packages = with pkgs; [
    nixd
    coq
    deno
    sbcl
    clisp
    (haskellPackages.ghcWithPackages (p: [ p.cabal-install ]))
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test
    nodePackages.vscode-json-languageserver
    nodePackages.mermaid-cli
    pkgs-unstable.lean4
    # rust
    rustc
    cargo
    gcc
  ];
}
