{ pkgs, ... }:
{
  imports = [
    ../julia
    ../python
  ];

  home.packages = with pkgs; [
    nixd
    coq
    deno
    sbcl
    clisp
    (haskellPackages.ghcWithPackages (p: [ p.cabal-install ]))
    vscode-json-languageserver
    mermaid-cli
    unstable.lean4
    # rust
    rustc
    cargo
    gcc
  ];
}
